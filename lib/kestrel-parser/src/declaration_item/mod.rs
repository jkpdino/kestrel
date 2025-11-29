use chumsky::prelude::*;
use kestrel_lexer::Token;
use kestrel_span::Span;
use kestrel_syntax_tree::{SyntaxKind, SyntaxNode};

use crate::module::{ModuleDeclaration, parse_module_declaration};
use crate::import::{ImportDeclaration, parse_import_declaration};
use crate::class::{ClassDeclaration, parse_class_declaration};
use crate::protocol::{ProtocolDeclaration, parse_protocol_declaration};
use crate::r#struct::{StructDeclaration, parse_struct_declaration};
use crate::field::{FieldDeclaration, parse_field_declaration};
use crate::function::{FunctionDeclaration, parse_function_declaration};
use crate::type_alias::{TypeAliasDeclaration, parse_type_alias_declaration};
use crate::event::EventSink;
use crate::common::{
    module_declaration_parser_internal,
    visibility_parser_internal, import_declaration_parser_internal,
};
use crate::ty::{ty_parser, TyVariant};
use crate::function::ParameterData;

/// Represents a declaration item - a top-level unit of code in a Kestrel file
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclarationItem {
    Module(ModuleDeclaration),
    Import(ImportDeclaration),
    Class(ClassDeclaration),
    Protocol(ProtocolDeclaration),
    Struct(StructDeclaration),
    Field(FieldDeclaration),
    Function(FunctionDeclaration),
    TypeAlias(TypeAliasDeclaration),
}

impl DeclarationItem {
    /// Get the span of this declaration item
    pub fn span(&self) -> &Span {
        match self {
            DeclarationItem::Module(decl) => &decl.span,
            DeclarationItem::Import(decl) => &decl.span,
            DeclarationItem::Class(decl) => &decl.span,
            DeclarationItem::Protocol(decl) => &decl.span,
            DeclarationItem::Struct(decl) => &decl.span,
            DeclarationItem::Field(decl) => &decl.span,
            DeclarationItem::Function(decl) => &decl.span,
            DeclarationItem::TypeAlias(decl) => &decl.span,
        }
    }

    /// Get the syntax tree for this declaration item
    pub fn syntax(&self) -> &SyntaxNode {
        match self {
            DeclarationItem::Module(decl) => &decl.syntax,
            DeclarationItem::Import(decl) => &decl.syntax,
            DeclarationItem::Class(decl) => &decl.syntax,
            DeclarationItem::Protocol(decl) => &decl.syntax,
            DeclarationItem::Struct(decl) => &decl.syntax,
            DeclarationItem::Field(decl) => &decl.syntax,
            DeclarationItem::Function(decl) => &decl.syntax,
            DeclarationItem::TypeAlias(decl) => &decl.syntax,
        }
    }
}

/// Raw parsed data for a declaration item
#[derive(Debug, Clone)]
enum DeclarationItemData {
    Module(Span, Vec<Span>),
    Import(Span, Vec<Span>, Option<Span>, Option<Vec<(Span, Option<Span>)>>),
    Class(Option<(Token, Span)>, Span, Span, Span, Vec<DeclarationItemData>, Span),
    /// Protocol: visibility, protocol_span, name_span, lbrace_span, body (functions), rbrace_span
    Protocol(Option<(Token, Span)>, Span, Span, Span, Vec<DeclarationItemData>, Span),
    Struct(Option<(Token, Span)>, Span, Span, Span, Vec<DeclarationItemData>, Span),
    /// Field: visibility, is_static, mutability_span, is_mutable, name_span, colon_span, ty
    Field(Option<(Token, Span)>, Option<Span>, Span, bool, Span, Span, TyVariant),
    /// Function: visibility, is_static, fn_span, name_span, lparen, parameters, rparen, return_type, body (optional)
    Function(Option<(Token, Span)>, Option<Span>, Span, Span, Span, Vec<ParameterData>, Span, Option<(Span, TyVariant)>, Option<(Span, Span)>),
    TypeAlias(Option<(Token, Span)>, Span, Span, Span, Span, Span),
}

/// Static modifier parser
fn static_parser_internal() -> impl Parser<Token, Option<Span>, Error = Simple<Token>> + Clone {
    use crate::common::token;
    token(Token::Static)
        .map(Some)
        .or(empty().map(|_| None))
}

/// Let/var parser - returns (span, is_mutable)
fn let_var_parser_internal() -> impl Parser<Token, (Span, bool), Error = Simple<Token>> + Clone {
    use crate::common::token;
    token(Token::Let)
        .map(|span| (span, false))
        .or(token(Token::Var).map(|span| (span, true)))
}

/// Parser for a single parameter: (label)? bind_name: Type
fn parameter_parser_internal() -> impl Parser<Token, ParameterData, Error = Simple<Token>> + Clone {
    use crate::common::{skip_trivia, identifier};

    // Try to parse: identifier identifier : type (labeled parameter)
    // Or: identifier : type (unlabeled parameter)
    skip_trivia()
        .ignore_then(identifier())
        .then(
            // Optional second identifier (if present, first was label)
            identifier().or_not()
        )
        .then(
            just(Token::Colon).map_with_span(|_, span| span)
        )
        .then(ty_parser())
        .map(|(((first_ident, second_ident_opt), colon), ty)| {
            match second_ident_opt {
                Some(second_ident) => {
                    // Two identifiers: first is label, second is bind_name
                    ParameterData {
                        label: Some(first_ident),
                        bind_name: second_ident,
                        colon,
                        ty,
                    }
                }
                None => {
                    // One identifier: it's the bind_name, no label
                    ParameterData {
                        label: None,
                        bind_name: first_ident,
                        colon,
                        ty,
                    }
                }
            }
        })
}

/// Parser for parameter list (zero or more parameters separated by commas)
fn parameter_list_parser_internal() -> impl Parser<Token, Vec<ParameterData>, Error = Simple<Token>> + Clone {
    use crate::common::skip_trivia;

    skip_trivia()
        .ignore_then(
            parameter_parser_internal()
                .separated_by(just(Token::Comma).map_with_span(|_, span| span))
                .allow_trailing()
        )
}

/// Parser for optional return type: -> Type
fn return_type_parser_internal() -> impl Parser<Token, Option<(Span, TyVariant)>, Error = Simple<Token>> + Clone {
    use crate::common::skip_trivia;

    skip_trivia()
        .ignore_then(just(Token::Arrow).map_with_span(|_, span| span))
        .then(ty_parser())
        .map(|(arrow, ty)| (arrow, ty))
        .or_not()
}

/// Internal Chumsky parser for a single declaration item
fn declaration_item_parser_internal() -> impl Parser<Token, DeclarationItemData, Error = Simple<Token>> + Clone {
    use crate::common::{token, identifier};

    recursive(|declaration_item| {
        let module_parser = module_declaration_parser_internal()
            .map(|(span, path)| DeclarationItemData::Module(span, path));

        let import_parser = import_declaration_parser_internal()
            .map(|(import_span, path, alias, items)| DeclarationItemData::Import(import_span, path, alias, items));

        let class_parser = visibility_parser_internal()
            .then(token(Token::Class))
            .then(identifier())
            .then(token(Token::LBrace))
            .then(declaration_item.clone().repeated())
            .then(token(Token::RBrace))
            .map(|(((((visibility, class_span), name_span), lbrace_span), body), rbrace_span)| {
                DeclarationItemData::Class(visibility, class_span, name_span, lbrace_span, body, rbrace_span)
            });

        let struct_parser = visibility_parser_internal()
            .then(token(Token::Struct))
            .then(identifier())
            .then(token(Token::LBrace))
            .then(declaration_item.clone().repeated())
            .then(token(Token::RBrace))
            .map(|(((((visibility, struct_span), name_span), lbrace_span), body), rbrace_span)| {
                DeclarationItemData::Struct(visibility, struct_span, name_span, lbrace_span, body, rbrace_span)
            });

        // Protocol: (visibility)? protocol Name { (function declarations)* }
        // Protocol body only contains function declarations (methods)
        let protocol_parser = visibility_parser_internal()
            .then(token(Token::Protocol))
            .then(identifier())
            .then(token(Token::LBrace))
            .then(declaration_item.clone().repeated())
            .then(token(Token::RBrace))
            .map(|(((((visibility, protocol_span), name_span), lbrace_span), body), rbrace_span)| {
                DeclarationItemData::Protocol(visibility, protocol_span, name_span, lbrace_span, body, rbrace_span)
            });

        // Field: (visibility)? (static)? let/var name: Type
        let field_parser = visibility_parser_internal()
            .then(static_parser_internal())
            .then(let_var_parser_internal())
            .then(identifier())
            .then(token(Token::Colon))
            .then(ty_parser())
            .map(|(((((visibility, is_static), (mutability_span, is_mutable)), name_span), colon_span), ty)| {
                DeclarationItemData::Field(visibility, is_static, mutability_span, is_mutable, name_span, colon_span, ty)
            });

        // Function body parser (optional): { }
        let function_body_parser = token(Token::LBrace)
            .then(token(Token::RBrace))
            .map(|(lbrace, rbrace)| Some((lbrace, rbrace)))
            .or(empty().map(|_| None));

        // Function: (visibility)? (static)? fn name(params) (-> Type)? ({ })?
        let function_parser = visibility_parser_internal()
            .then(static_parser_internal())
            .then(token(Token::Func))
            .then(identifier())
            .then(token(Token::LParen))
            .then(parameter_list_parser_internal())
            .then(token(Token::RParen))
            .then(return_type_parser_internal())
            .then(function_body_parser)
            .map(|((((((((visibility, is_static), fn_span), name_span), lparen), parameters), rparen), return_type), body)| {
                DeclarationItemData::Function(visibility, is_static, fn_span, name_span, lparen, parameters, rparen, return_type, body)
            });

        let type_alias_parser = visibility_parser_internal()
            .then(token(Token::Type))
            .then(identifier())
            .then(token(Token::Equals))
            .then(identifier())
            .then(token(Token::Semicolon))
            .map(|(((((visibility, type_span), name_span), equals_span), aliased_type_span), semicolon_span)| {
                DeclarationItemData::TypeAlias(visibility, type_span, name_span, equals_span, aliased_type_span, semicolon_span)
            });

        module_parser.or(import_parser).or(class_parser).or(protocol_parser).or(struct_parser).or(function_parser).or(field_parser).or(type_alias_parser)
    })
}

/// Internal Chumsky parser for multiple declaration items with error recovery
///
/// This parser attempts to parse multiple declarations. When a single declaration fails,
/// Chumsky's error recovery will collect the error and the `.repeated()` combinator will
/// continue attempting to parse subsequent declarations. This allows the parser to report
/// multiple errors in a single pass.
fn declaration_items_parser_internal() -> impl Parser<Token, Vec<DeclarationItemData>, Error = Simple<Token>> + Clone {
    use crate::common::skip_trivia;

    declaration_item_parser_internal()
        .repeated()
        .at_least(0)
        .then_ignore(skip_trivia()) // Skip any trailing trivia
}

/// Parse a declaration item and emit events
/// This is the primary event-driven parser function
/// Tries to parse as a module declaration first, then import, then class
pub fn parse_declaration_item<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    // Clone the iterator so we can try multiple parsers
    let tokens_clone1 = tokens.clone();
    let tokens_clone2 = tokens.clone();
    let tokens_clone3 = tokens.clone();
    let tokens_clone4 = tokens.clone();
    let tokens_clone5 = tokens.clone();
    let tokens_clone6 = tokens.clone();
    let tokens_clone7 = tokens.clone();

    // Try parsing as module declaration
    let module_result = {
        let mut temp_sink = EventSink::new();
        parse_module_declaration(source, tokens, &mut temp_sink);

        // Check if there were errors
        let has_errors = temp_sink.events().iter().any(|e| matches!(e, crate::event::Event::Error { .. }));
        if !has_errors {
            // Success! Copy events to the main sink
            for event in temp_sink.into_events() {
                match event {
                    crate::event::Event::StartNode(kind) => sink.start_node(kind),
                    crate::event::Event::AddToken(kind, span) => sink.add_token(kind, span),
                    crate::event::Event::FinishNode => sink.finish_node(),
                    crate::event::Event::Error { message, span } => sink.error(message, span),
                }
            }
            return;
        }
        has_errors
    };

    // If module parsing failed, try import declaration
    if module_result {
        let mut temp_sink = EventSink::new();
        parse_import_declaration(source, tokens_clone1, &mut temp_sink);

        // Check if there were errors
        let has_errors = temp_sink.events().iter().any(|e| matches!(e, crate::event::Event::Error { .. }));
        if !has_errors {
            // Success! Copy events to the main sink
            for event in temp_sink.into_events() {
                match event {
                    crate::event::Event::StartNode(kind) => sink.start_node(kind),
                    crate::event::Event::AddToken(kind, span) => sink.add_token(kind, span),
                    crate::event::Event::FinishNode => sink.finish_node(),
                    crate::event::Event::Error { message, span } => sink.error(message, span),
                }
            }
            return;
        }
    }

    // If import parsing failed, try class declaration
    let mut temp_sink = EventSink::new();
    parse_class_declaration(source, tokens_clone2, &mut temp_sink);

    // Check if there were errors
    let has_errors = temp_sink.events().iter().any(|e| matches!(e, crate::event::Event::Error { .. }));
    if !has_errors {
        // Success! Copy events to the main sink
        for event in temp_sink.into_events() {
            match event {
                crate::event::Event::StartNode(kind) => sink.start_node(kind),
                crate::event::Event::AddToken(kind, span) => sink.add_token(kind, span),
                crate::event::Event::FinishNode => sink.finish_node(),
                crate::event::Event::Error { message, span } => sink.error(message, span),
            }
        }
        return;
    }

    // If class parsing failed, try protocol declaration
    {
        let mut temp_sink = EventSink::new();
        parse_protocol_declaration(source, tokens_clone3, &mut temp_sink);

        // Check if there were errors
        let has_errors = temp_sink.events().iter().any(|e| matches!(e, crate::event::Event::Error { .. }));
        if !has_errors {
            // Success! Copy events to the main sink
            for event in temp_sink.into_events() {
                match event {
                    crate::event::Event::StartNode(kind) => sink.start_node(kind),
                    crate::event::Event::AddToken(kind, span) => sink.add_token(kind, span),
                    crate::event::Event::FinishNode => sink.finish_node(),
                    crate::event::Event::Error { message, span } => sink.error(message, span),
                }
            }
            return;
        }
    }

    // If protocol parsing failed, try struct declaration
    {
        let mut temp_sink = EventSink::new();
        parse_struct_declaration(source, tokens_clone4, &mut temp_sink);

        // Check if there were errors
        let has_errors = temp_sink.events().iter().any(|e| matches!(e, crate::event::Event::Error { .. }));
        if !has_errors {
            // Success! Copy events to the main sink
            for event in temp_sink.into_events() {
                match event {
                    crate::event::Event::StartNode(kind) => sink.start_node(kind),
                    crate::event::Event::AddToken(kind, span) => sink.add_token(kind, span),
                    crate::event::Event::FinishNode => sink.finish_node(),
                    crate::event::Event::Error { message, span } => sink.error(message, span),
                }
            }
            return;
        }
    }

    // If struct parsing failed, try function declaration
    {
        let mut temp_sink = EventSink::new();
        parse_function_declaration(source, tokens_clone5, &mut temp_sink);

        // Check if there were errors
        let has_errors = temp_sink.events().iter().any(|e| matches!(e, crate::event::Event::Error { .. }));
        if !has_errors {
            // Success! Copy events to the main sink
            for event in temp_sink.into_events() {
                match event {
                    crate::event::Event::StartNode(kind) => sink.start_node(kind),
                    crate::event::Event::AddToken(kind, span) => sink.add_token(kind, span),
                    crate::event::Event::FinishNode => sink.finish_node(),
                    crate::event::Event::Error { message, span } => sink.error(message, span),
                }
            }
            return;
        }
    }

    // If function parsing failed, try field declaration
    {
        let mut temp_sink = EventSink::new();
        parse_field_declaration(source, tokens_clone6, &mut temp_sink);

        // Check if there were errors
        let has_errors = temp_sink.events().iter().any(|e| matches!(e, crate::event::Event::Error { .. }));
        if !has_errors {
            // Success! Copy events to the main sink
            for event in temp_sink.into_events() {
                match event {
                    crate::event::Event::StartNode(kind) => sink.start_node(kind),
                    crate::event::Event::AddToken(kind, span) => sink.add_token(kind, span),
                    crate::event::Event::FinishNode => sink.finish_node(),
                    crate::event::Event::Error { message, span } => sink.error(message, span),
                }
            }
            return;
        }
    }

    // If field parsing failed, try type alias declaration
    let mut temp_sink = EventSink::new();
    parse_type_alias_declaration(source, tokens_clone7, &mut temp_sink);

    // Check if there were errors
    let has_errors = temp_sink.events().iter().any(|e| matches!(e, crate::event::Event::Error { .. }));
    if !has_errors {
        // Success! Copy events to the main sink
        for event in temp_sink.into_events() {
            match event {
                crate::event::Event::StartNode(kind) => sink.start_node(kind),
                crate::event::Event::AddToken(kind, span) => sink.add_token(kind, span),
                crate::event::Event::FinishNode => sink.finish_node(),
                crate::event::Event::Error { message, span } => sink.error(message, span),
            }
        }
        return;
    }

    // All failed - emit error (no specific span available since all parsers failed)
    sink.error_no_span("Expected module, import, class, protocol, struct, function, field, or type alias declaration".to_string());
}

/// Parse a source file (multiple declaration items) and emit events
/// This creates a SourceFile root node containing all declarations
pub fn parse_source_file<I>(source: &str, tokens: I, sink: &mut EventSink)
where
    I: Iterator<Item = (Token, Span)> + Clone,
{
    let end_pos = source.len();
    let stream = chumsky::Stream::from_iter(end_pos..end_pos, tokens);

    sink.start_node(SyntaxKind::SourceFile);

    match declaration_items_parser_internal().parse(stream) {
        Ok(items) => {
            // Emit events for each declaration item
            for item_data in items {
                match item_data {
                    DeclarationItemData::Module(module_span, path_segments) => {
                        // Emit module declaration events
                        sink.start_node(SyntaxKind::ModuleDeclaration);
                        sink.add_token(SyntaxKind::Module, module_span);
                        crate::common::emit_module_path(sink, &path_segments);
                        sink.finish_node();
                    }
                    DeclarationItemData::Import(import_span, path_segments, alias, items) => {
                        // Emit import declaration events
                        crate::common::emit_import_declaration(sink, import_span, &path_segments, alias, items);
                    }
                    DeclarationItemData::Class(visibility, class_span, name_span, lbrace_span, body, rbrace_span) => {
                        // Emit class declaration events
                        emit_class_declaration(sink, visibility, class_span, name_span, lbrace_span, body, rbrace_span);
                    }
                    DeclarationItemData::Protocol(visibility, protocol_span, name_span, lbrace_span, body, rbrace_span) => {
                        // Emit protocol declaration events
                        emit_protocol_declaration(sink, visibility, protocol_span, name_span, lbrace_span, body, rbrace_span);
                    }
                    DeclarationItemData::Struct(visibility, struct_span, name_span, lbrace_span, body, rbrace_span) => {
                        // Emit struct declaration events
                        emit_struct_declaration(sink, visibility, struct_span, name_span, lbrace_span, body, rbrace_span);
                    }
                    DeclarationItemData::Field(visibility, is_static, mutability_span, is_mutable, name_span, colon_span, ty) => {
                        // Emit field declaration events
                        emit_field_declaration(sink, visibility, is_static, mutability_span, is_mutable, name_span, colon_span, ty);
                    }
                    DeclarationItemData::Function(visibility, is_static, fn_span, name_span, lparen, parameters, rparen, return_type, body) => {
                        // Emit function declaration events
                        emit_function_declaration(sink, visibility, is_static, fn_span, name_span, lparen, parameters, rparen, return_type, body);
                    }
                    DeclarationItemData::TypeAlias(visibility, type_span, name_span, equals_span, aliased_type_span, semicolon_span) => {
                        // Emit type alias declaration events
                        emit_type_alias_declaration(sink, visibility, type_span, name_span, equals_span, aliased_type_span, semicolon_span);
                    }
                }
            }
        }
        Err(errors) => {
            // Emit error events for each parse error
            for error in errors {
                // Chumsky errors have span information
                let span = error.span();
                sink.error_at(format!("Parse error: {:?}", error), span);
            }
        }
    }

    sink.finish_node();
}

/// Emit events for a class declaration
/// Helper function used by parse_source_file
fn emit_class_declaration(
    sink: &mut EventSink,
    visibility: Option<(Token, Span)>,
    class_span: Span,
    name_span: Span,
    lbrace_span: Span,
    body: Vec<DeclarationItemData>,
    rbrace_span: Span,
) {
    sink.start_node(SyntaxKind::ClassDeclaration);

    // Always emit Visibility node (may be empty)
    sink.start_node(SyntaxKind::Visibility);
    if let Some((vis_token, vis_span)) = visibility {
        let vis_kind = match vis_token {
            Token::Public => SyntaxKind::Public,
            Token::Private => SyntaxKind::Private,
            Token::Internal => SyntaxKind::Internal,
            Token::Fileprivate => SyntaxKind::Fileprivate,
            _ => unreachable!("visibility_parser_internal only returns visibility tokens"),
        };
        sink.add_token(vis_kind, vis_span);
    }
    sink.finish_node(); // Finish Visibility

    sink.add_token(SyntaxKind::Class, class_span);

    // Emit Name node wrapping the identifier
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, name_span);
    sink.finish_node(); // Finish Name

    // Emit ClassBody node wrapping the body content
    sink.start_node(SyntaxKind::ClassBody);
    sink.add_token(SyntaxKind::LBrace, lbrace_span);

    // Emit nested declaration items
    for item_data in body {
        emit_declaration_item_internal(sink, item_data);
    }

    sink.add_token(SyntaxKind::RBrace, rbrace_span);
    sink.finish_node(); // Finish ClassBody

    sink.finish_node(); // Finish ClassDeclaration
}

/// Emit events for a protocol declaration
/// Helper function used by parse_source_file
fn emit_protocol_declaration(
    sink: &mut EventSink,
    visibility: Option<(Token, Span)>,
    protocol_span: Span,
    name_span: Span,
    lbrace_span: Span,
    body: Vec<DeclarationItemData>,
    rbrace_span: Span,
) {
    sink.start_node(SyntaxKind::ProtocolDeclaration);

    // Always emit Visibility node (may be empty)
    sink.start_node(SyntaxKind::Visibility);
    if let Some((vis_token, vis_span)) = visibility {
        let vis_kind = match vis_token {
            Token::Public => SyntaxKind::Public,
            Token::Private => SyntaxKind::Private,
            Token::Internal => SyntaxKind::Internal,
            Token::Fileprivate => SyntaxKind::Fileprivate,
            _ => unreachable!("visibility_parser_internal only returns visibility tokens"),
        };
        sink.add_token(vis_kind, vis_span);
    }
    sink.finish_node(); // Finish Visibility

    sink.add_token(SyntaxKind::Protocol, protocol_span);

    // Emit Name node wrapping the identifier
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, name_span);
    sink.finish_node(); // Finish Name

    // Emit ProtocolBody node wrapping the body content
    sink.start_node(SyntaxKind::ProtocolBody);
    sink.add_token(SyntaxKind::LBrace, lbrace_span);

    // Emit nested declaration items (should only be function declarations)
    for item_data in body {
        emit_declaration_item_internal(sink, item_data);
    }

    sink.add_token(SyntaxKind::RBrace, rbrace_span);
    sink.finish_node(); // Finish ProtocolBody

    sink.finish_node(); // Finish ProtocolDeclaration
}

/// Emit events for a struct declaration
/// Helper function used by parse_source_file
fn emit_struct_declaration(
    sink: &mut EventSink,
    visibility: Option<(Token, Span)>,
    struct_span: Span,
    name_span: Span,
    lbrace_span: Span,
    body: Vec<DeclarationItemData>,
    rbrace_span: Span,
) {
    sink.start_node(SyntaxKind::StructDeclaration);

    // Always emit Visibility node (may be empty)
    sink.start_node(SyntaxKind::Visibility);
    if let Some((vis_token, vis_span)) = visibility {
        let vis_kind = match vis_token {
            Token::Public => SyntaxKind::Public,
            Token::Private => SyntaxKind::Private,
            Token::Internal => SyntaxKind::Internal,
            Token::Fileprivate => SyntaxKind::Fileprivate,
            _ => unreachable!("visibility_parser_internal only returns visibility tokens"),
        };
        sink.add_token(vis_kind, vis_span);
    }
    sink.finish_node(); // Finish Visibility

    sink.add_token(SyntaxKind::Struct, struct_span);

    // Emit Name node wrapping the identifier
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, name_span);
    sink.finish_node(); // Finish Name

    // Emit StructBody node wrapping the body content
    sink.start_node(SyntaxKind::StructBody);
    sink.add_token(SyntaxKind::LBrace, lbrace_span);

    // Emit nested declaration items
    for item_data in body {
        emit_declaration_item_internal(sink, item_data);
    }

    sink.add_token(SyntaxKind::RBrace, rbrace_span);
    sink.finish_node(); // Finish StructBody

    sink.finish_node(); // Finish StructDeclaration
}

/// Emit events for a field declaration
/// Helper function used by parse_source_file
fn emit_field_declaration(
    sink: &mut EventSink,
    visibility: Option<(Token, Span)>,
    is_static: Option<Span>,
    mutability_span: Span,
    is_mutable: bool,
    name_span: Span,
    colon_span: Span,
    ty: TyVariant,
) {
    use crate::ty::{emit_unit_type, emit_never_type, emit_tuple_type, emit_function_type, emit_path_type};

    sink.start_node(SyntaxKind::FieldDeclaration);

    // Always emit Visibility node (may be empty)
    sink.start_node(SyntaxKind::Visibility);
    if let Some((vis_token, vis_span)) = visibility {
        let vis_kind = match vis_token {
            Token::Public => SyntaxKind::Public,
            Token::Private => SyntaxKind::Private,
            Token::Internal => SyntaxKind::Internal,
            Token::Fileprivate => SyntaxKind::Fileprivate,
            _ => unreachable!("visibility_parser_internal only returns visibility tokens"),
        };
        sink.add_token(vis_kind, vis_span);
    }
    sink.finish_node(); // Finish Visibility

    // Emit StaticModifier node if present
    if let Some(static_span) = is_static {
        sink.start_node(SyntaxKind::StaticModifier);
        sink.add_token(SyntaxKind::Static, static_span);
        sink.finish_node();
    }

    // Emit let or var keyword
    if is_mutable {
        sink.add_token(SyntaxKind::Var, mutability_span);
    } else {
        sink.add_token(SyntaxKind::Let, mutability_span);
    }

    // Emit Name node wrapping the identifier
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, name_span);
    sink.finish_node(); // Finish Name

    sink.add_token(SyntaxKind::Colon, colon_span);

    // Emit the type
    match ty {
        TyVariant::Unit(lparen, rparen) => emit_unit_type(sink, lparen, rparen),
        TyVariant::Never(bang) => emit_never_type(sink, bang),
        TyVariant::Tuple(lparen, types, rparen) => emit_tuple_type(sink, lparen, types, rparen),
        TyVariant::Function(lparen, params, rparen, arrow, ret) => {
            emit_function_type(sink, lparen, params, rparen, arrow, ret)
        }
        TyVariant::Path(segments) => emit_path_type(sink, &segments),
    }

    sink.finish_node(); // Finish FieldDeclaration
}

/// Emit events for a function declaration
/// Helper function used by parse_source_file
fn emit_function_declaration(
    sink: &mut EventSink,
    visibility: Option<(Token, Span)>,
    is_static: Option<Span>,
    fn_span: Span,
    name_span: Span,
    lparen: Span,
    parameters: Vec<ParameterData>,
    rparen: Span,
    return_type: Option<(Span, TyVariant)>,
    body: Option<(Span, Span)>,
) {
    use crate::ty::{emit_unit_type, emit_never_type, emit_tuple_type, emit_function_type, emit_path_type};

    sink.start_node(SyntaxKind::FunctionDeclaration);

    // Always emit Visibility node (may be empty)
    sink.start_node(SyntaxKind::Visibility);
    if let Some((vis_token, vis_span)) = visibility {
        let vis_kind = match vis_token {
            Token::Public => SyntaxKind::Public,
            Token::Private => SyntaxKind::Private,
            Token::Internal => SyntaxKind::Internal,
            Token::Fileprivate => SyntaxKind::Fileprivate,
            _ => unreachable!("visibility_parser_internal only returns visibility tokens"),
        };
        sink.add_token(vis_kind, vis_span);
    }
    sink.finish_node(); // Finish Visibility

    // Emit StaticModifier node if present
    if let Some(static_span) = is_static {
        sink.start_node(SyntaxKind::StaticModifier);
        sink.add_token(SyntaxKind::Static, static_span);
        sink.finish_node();
    }

    // Emit fn keyword
    sink.add_token(SyntaxKind::Func, fn_span);

    // Emit Name node wrapping the identifier
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, name_span);
    sink.finish_node(); // Finish Name

    // Emit ParameterList node
    sink.start_node(SyntaxKind::ParameterList);
    sink.add_token(SyntaxKind::LParen, lparen);

    for param in parameters {
        sink.start_node(SyntaxKind::Parameter);

        // Emit label if present (as a Name node)
        if let Some(label_span) = param.label {
            sink.start_node(SyntaxKind::Name);
            sink.add_token(SyntaxKind::Identifier, label_span);
            sink.finish_node();
        }

        // Emit bind_name (as a Name node)
        sink.start_node(SyntaxKind::Name);
        sink.add_token(SyntaxKind::Identifier, param.bind_name);
        sink.finish_node();

        sink.add_token(SyntaxKind::Colon, param.colon);

        // Emit the type
        match param.ty {
            TyVariant::Unit(lp, rp) => emit_unit_type(sink, lp, rp),
            TyVariant::Never(bang) => emit_never_type(sink, bang),
            TyVariant::Tuple(lp, types, rp) => emit_tuple_type(sink, lp, types, rp),
            TyVariant::Function(lp, params, rp, arrow, ret) => {
                emit_function_type(sink, lp, params, rp, arrow, ret)
            }
            TyVariant::Path(segments) => emit_path_type(sink, &segments),
        }

        sink.finish_node(); // Finish Parameter
    }

    sink.add_token(SyntaxKind::RParen, rparen);
    sink.finish_node(); // Finish ParameterList

    // Emit ReturnType node if present
    if let Some((arrow_span, return_ty)) = return_type {
        sink.start_node(SyntaxKind::ReturnType);
        sink.add_token(SyntaxKind::Arrow, arrow_span);

        // Emit the return type
        match return_ty {
            TyVariant::Unit(lp, rp) => emit_unit_type(sink, lp, rp),
            TyVariant::Never(bang) => emit_never_type(sink, bang),
            TyVariant::Tuple(lp, types, rp) => emit_tuple_type(sink, lp, types, rp),
            TyVariant::Function(lp, params, rp, arrow, ret) => {
                emit_function_type(sink, lp, params, rp, arrow, ret)
            }
            TyVariant::Path(segments) => emit_path_type(sink, &segments),
        }

        sink.finish_node(); // Finish ReturnType
    }

    // Emit FunctionBody node if present (optional for protocol methods)
    if let Some((lbrace, rbrace)) = body {
        sink.start_node(SyntaxKind::FunctionBody);
        sink.add_token(SyntaxKind::LBrace, lbrace);
        // TODO: Function body contents will be added later
        sink.add_token(SyntaxKind::RBrace, rbrace);
        sink.finish_node(); // Finish FunctionBody
    }

    sink.finish_node(); // Finish FunctionDeclaration
}

/// Emit events for a type alias declaration
/// Helper function used by parse_source_file
fn emit_type_alias_declaration(
    sink: &mut EventSink,
    visibility: Option<(Token, Span)>,
    type_span: Span,
    name_span: Span,
    equals_span: Span,
    aliased_type_span: Span,
    semicolon_span: Span,
) {
    sink.start_node(SyntaxKind::TypeAliasDeclaration);

    // Always emit Visibility node (may be empty)
    sink.start_node(SyntaxKind::Visibility);
    if let Some((vis_token, vis_span)) = visibility {
        let vis_kind = match vis_token {
            Token::Public => SyntaxKind::Public,
            Token::Private => SyntaxKind::Private,
            Token::Internal => SyntaxKind::Internal,
            Token::Fileprivate => SyntaxKind::Fileprivate,
            _ => unreachable!("visibility_parser_internal only returns visibility tokens"),
        };
        sink.add_token(vis_kind, vis_span);
    }
    sink.finish_node(); // Finish Visibility

    sink.add_token(SyntaxKind::Type, type_span);

    // Emit Name node wrapping the identifier
    sink.start_node(SyntaxKind::Name);
    sink.add_token(SyntaxKind::Identifier, name_span);
    sink.finish_node(); // Finish Name

    sink.add_token(SyntaxKind::Equals, equals_span);

    // Emit AliasedType node wrapping the aliased type identifier
    sink.start_node(SyntaxKind::AliasedType);
    sink.add_token(SyntaxKind::Identifier, aliased_type_span);
    sink.finish_node(); // Finish AliasedType

    sink.add_token(SyntaxKind::Semicolon, semicolon_span);

    sink.finish_node(); // Finish TypeAliasDeclaration
}

/// Emit events for a declaration item (internal recursive helper)
/// Helper function used by emit_class_declaration
fn emit_declaration_item_internal(sink: &mut EventSink, item_data: DeclarationItemData) {
    match item_data {
        DeclarationItemData::Module(module_span, path_segments) => {
            sink.start_node(SyntaxKind::ModuleDeclaration);
            sink.add_token(SyntaxKind::Module, module_span);
            crate::common::emit_module_path(sink, &path_segments);
            sink.finish_node();
        }
        DeclarationItemData::Import(import_span, path_segments, alias, items) => {
            crate::common::emit_import_declaration(sink, import_span, &path_segments, alias, items);
        }
        DeclarationItemData::Class(visibility, class_span, name_span, lbrace_span, body, rbrace_span) => {
            emit_class_declaration(sink, visibility, class_span, name_span, lbrace_span, body, rbrace_span);
        }
        DeclarationItemData::Protocol(visibility, protocol_span, name_span, lbrace_span, body, rbrace_span) => {
            emit_protocol_declaration(sink, visibility, protocol_span, name_span, lbrace_span, body, rbrace_span);
        }
        DeclarationItemData::Struct(visibility, struct_span, name_span, lbrace_span, body, rbrace_span) => {
            emit_struct_declaration(sink, visibility, struct_span, name_span, lbrace_span, body, rbrace_span);
        }
        DeclarationItemData::Field(visibility, is_static, mutability_span, is_mutable, name_span, colon_span, ty) => {
            emit_field_declaration(sink, visibility, is_static, mutability_span, is_mutable, name_span, colon_span, ty);
        }
        DeclarationItemData::Function(visibility, is_static, fn_span, name_span, lparen, parameters, rparen, return_type, body) => {
            emit_function_declaration(sink, visibility, is_static, fn_span, name_span, lparen, parameters, rparen, return_type, body);
        }
        DeclarationItemData::TypeAlias(visibility, type_span, name_span, equals_span, aliased_type_span, semicolon_span) => {
            emit_type_alias_declaration(sink, visibility, type_span, name_span, equals_span, aliased_type_span, semicolon_span);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use kestrel_lexer::lex;
    use kestrel_syntax_tree::SyntaxKind;

    #[test]
    fn test_declaration_item_module() {
        let source = "module A.B.C";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_declaration_item(source, tokens.into_iter(), &mut sink);

        // Check that we got module events
        let events = sink.events();
        let has_module = events.iter().any(|e| {
            matches!(e, crate::event::Event::StartNode(kind) if *kind == SyntaxKind::ModuleDeclaration)
        });
        assert!(has_module, "Should have parsed as module declaration");
    }

    #[test]
    fn test_declaration_item_single_module() {
        let source = "module Main";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_declaration_item(source, tokens.into_iter(), &mut sink);

        // Check that we got module events
        let events = sink.events();
        let has_module = events.iter().any(|e| {
            matches!(e, crate::event::Event::StartNode(kind) if *kind == SyntaxKind::ModuleDeclaration)
        });
        assert!(has_module, "Should have parsed as module declaration");
    }

    #[test]
    fn test_declaration_item_import() {
        let source = "import A.B.C";
        let tokens: Vec<_> = lex(source)
            .filter_map(|t| t.ok())
            .map(|spanned| (spanned.value, spanned.span))
            .collect::<Vec<_>>();

        let mut sink = EventSink::new();
        parse_declaration_item(source, tokens.into_iter(), &mut sink);

        // Check that we got import events
        let events = sink.events();
        let has_import = events.iter().any(|e| {
            matches!(e, crate::event::Event::StartNode(kind) if *kind == SyntaxKind::ImportDeclaration)
        });
        assert!(has_import, "Should have parsed as import declaration");
    }
}
