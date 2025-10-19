use std::sync::{Arc, RwLock, Weak};

use kestrel_span::{Name, Span};

use crate::{behavior::Behavior, language::Language, symbol::Symbol};

#[derive(Debug)]
pub struct SymbolMetadata<L: Language> {
    parent: Option<Weak<dyn Symbol<L>>>,
    children: RwLock<Vec<Arc<dyn Symbol<L>>>>,

    behaviors: RwLock<Vec<Arc<dyn Behavior<L>>>>,

    kind: L::SymbolKind,
    name: Name,
    declaration_span: Span,
    span: Span,
}

impl<L: Language> SymbolMetadata<L> {
    pub fn parent(&self) -> Option<Arc<dyn Symbol<L>>> {
        let symbol = self.parent.as_ref()?;

        return Some(
            symbol
                .upgrade()
                .expect("internal error: parent was deleted"),
        );
    }

    pub fn children(&self) -> Vec<Arc<dyn Symbol<L>>> {
        let Ok(children) = self.children.read() else {
            panic!("internal error: RwLock poison");
        };

        return children.clone();
    }

    pub fn add_child(&self, child: &Arc<dyn Symbol<L>>) {
        let Ok(mut children) = self.children.write() else {
            panic!("internal error: RwLock poison");
        };

        children.push(child.clone());
    }

    pub fn kind(&self) -> L::SymbolKind {
        return self.kind;
    }

    pub fn name(&self) -> Name {
        return self.name.clone();
    }

    pub fn declaration_span(&self) -> Span {
        return self.declaration_span.clone();
    }

    pub fn span(&self) -> Span {
        return self.span.clone();
    }

    pub fn behaviors(&self) -> Vec<Arc<dyn Behavior<L>>> {
        let Ok(behaviors) = self.behaviors.read() else {
            panic!("internal error: RwLock poison");
        };

        return behaviors.clone();
    }
}

pub struct SymbolMetadataBuilder<L: Language> {
    parent: Option<Weak<dyn Symbol<L>>>,
    children: Vec<Arc<dyn Symbol<L>>>,
    behaviors: Vec<Arc<dyn Behavior<L>>>,
    kind: Option<L::SymbolKind>,
    name: Option<Name>,
    declaration_span: Option<Span>,
    span: Option<Span>,
}

impl<L: Language> SymbolMetadataBuilder<L> {
    pub fn new(kind: L::SymbolKind) -> Self {
        Self {
            parent: None,
            children: Vec::new(),
            behaviors: Vec::new(),
            kind: Some(kind),
            name: None,
            declaration_span: None,
            span: None,
        }
    }

    pub fn with_parent(mut self, parent: Weak<dyn Symbol<L>>) -> Self {
        self.parent = Some(parent);
        self
    }

    pub fn with_child(mut self, child: Arc<dyn Symbol<L>>) -> Self {
        self.children.push(child);
        self
    }

    pub fn with_children(mut self, children: Vec<Arc<dyn Symbol<L>>>) -> Self {
        self.children = children;
        self
    }

    pub fn with_behavior(mut self, behavior: Arc<dyn Behavior<L>>) -> Self {
        self.behaviors.push(behavior);
        self
    }

    pub fn with_behaviors(mut self, behaviors: Vec<Arc<dyn Behavior<L>>>) -> Self {
        self.behaviors = behaviors;
        self
    }

    pub fn with_name(mut self, name: Name) -> Self {
        self.name = Some(name);
        self
    }

    pub fn with_declaration_span(mut self, span: Span) -> Self {
        self.declaration_span = Some(span);
        self
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn build(self) -> SymbolMetadata<L> {
        SymbolMetadata {
            parent: self.parent,
            children: RwLock::new(self.children),
            behaviors: RwLock::new(self.behaviors),
            kind: self.kind.expect("kind is required"),
            name: self.name.expect("name is required"),
            declaration_span: self.declaration_span.expect("declaration_span is required"),
            span: self.span.expect("span is required"),
        }
    }
}
