use kestrel_semantic_tree::behavior::typed::TypedBehavior;
use kestrel_semantic_tree::behavior::visibility::{Visibility, VisibilityBehavior};
use kestrel_semantic_tree::language::KestrelLanguage;
use kestrel_semantic_tree::symbol::r#struct::StructSymbol;
use kestrel_semantic_tree::ty::Ty;
use kestrel_semantic_tree_builder::path_resolver::resolve_type_path;
use kestrel_span::Name;
use semantic_tree::symbol::{Symbol, SymbolMetadataBuilder, SymbolTable};
use std::sync::Arc;

fn main() {
    println!("=== Path Resolution Demo ===\n");

    // Create a root symbol and symbol table
    let root = create_root();
    let mut symbol_table = SymbolTable::new();

    println!("Setting up struct hierarchy:");
    println!("  Root");
    println!("    ├─ Graphics (public)");
    println!("    │  ├─ Shape (public)");
    println!("    │  │  └─ Circle (public)");
    println!("    │  └─ Color (private)");
    println!("    └─ Math (public)");
    println!("       └─ Vector (public)\n");

    // Create Graphics namespace
    let graphics = create_struct("Graphics", 0..100, Visibility::Public, &root);
    add_typed_behavior(&graphics, vec!["Graphics".to_string()]);

    // Create Graphics.Shape
    let shape = create_struct("Shape", 10..80, Visibility::Public, &root);
    add_typed_behavior(&shape, vec!["Graphics".to_string(), "Shape".to_string()]);
    graphics.metadata().add_child(&shape);

    // Create Graphics.Shape.Circle
    let circle = create_struct("Circle", 20..60, Visibility::Public, &root);
    add_typed_behavior(&circle, vec!["Graphics".to_string(), "Shape".to_string(), "Circle".to_string()]);
    shape.metadata().add_child(&circle);

    // Create Graphics.Color (private!)
    let color = create_struct("Color", 85..95, Visibility::Private, &graphics);
    add_typed_behavior(&color, vec!["Graphics".to_string(), "Color".to_string()]);
    graphics.metadata().add_child(&color);

    // Create Math namespace
    let math = create_struct("Math", 200..300, Visibility::Public, &root);
    add_typed_behavior(&math, vec!["Math".to_string()]);

    // Create Math.Vector
    let vector = create_struct("Vector", 210..280, Visibility::Public, &root);
    add_typed_behavior(&vector, vec!["Math".to_string(), "Vector".to_string()]);
    math.metadata().add_child(&vector);

    // Add top-level structs to symbol table
    symbol_table.insert(graphics.clone());
    symbol_table.insert(math.clone());
    root.metadata().add_child(&graphics);
    root.metadata().add_child(&math);

    // Test various path resolutions
    println!("Testing path resolutions from root context:\n");

    test_resolution(&symbol_table, &root, &["Graphics"], true);
    test_resolution(&symbol_table, &root, &["Graphics", "Shape"], true);
    test_resolution(&symbol_table, &root, &["Graphics", "Shape", "Circle"], true);
    test_resolution(&symbol_table, &root, &["Math", "Vector"], true);

    // These should fail
    test_resolution(&symbol_table, &root, &["Graphics", "Color"], false);  // Private!
    test_resolution(&symbol_table, &root, &["NonExistent"], false);
    test_resolution(&symbol_table, &root, &["Graphics", "NonExistent"], false);

    println!("\n=== Demo Complete ===");
}

fn create_root() -> Arc<dyn Symbol<KestrelLanguage>> {
    use kestrel_semantic_tree::symbol::kind::KestrelSymbolKind;

    let root_name = Name::new("Root".to_string(), 0..4);
    let metadata = SymbolMetadataBuilder::new(KestrelSymbolKind::Module)
        .with_name(root_name)
        .with_declaration_span(0..4)
        .with_span(0..1000)
        .build();

    #[derive(Debug)]
    struct RootSymbol {
        metadata: semantic_tree::symbol::SymbolMetadata<KestrelLanguage>,
    }

    impl Symbol<KestrelLanguage> for RootSymbol {
        fn metadata(&self) -> &semantic_tree::symbol::SymbolMetadata<KestrelLanguage> {
            &self.metadata
        }
    }

    Arc::new(RootSymbol { metadata })
}

fn create_struct(
    name: &str,
    span: std::ops::Range<usize>,
    visibility: Visibility,
    visibility_scope: &Arc<dyn Symbol<KestrelLanguage>>,
) -> Arc<dyn Symbol<KestrelLanguage>> {
    let struct_name = Name::new(name.to_string(), span.clone());
    let visibility_behavior = VisibilityBehavior::new(
        Some(visibility),
        span.start..span.start + 6,
        visibility_scope.clone(),
    );

    let struct_symbol = StructSymbol::new(struct_name, span, visibility_behavior, None);
    Arc::new(struct_symbol)
}

fn add_typed_behavior(symbol: &Arc<dyn Symbol<KestrelLanguage>>, _path: Vec<String>) {
    let span = symbol.metadata().span();
    // Use error type as placeholder - actual struct types would need downcasting
    let ty = Ty::error(span.clone());
    let typed_behavior = TypedBehavior::new(ty, span.clone());
    symbol.metadata().add_behavior(typed_behavior);
}

fn test_resolution(
    symbol_table: &SymbolTable<KestrelLanguage>,
    context: &Arc<dyn Symbol<KestrelLanguage>>,
    path: &[&str],
    should_succeed: bool,
) {
    let path_vec: Vec<String> = path.iter().map(|s| s.to_string()).collect();
    let path_str = path.join(".");

    let result = resolve_type_path(&path_vec, symbol_table, context);

    match result {
        Some(_ty) => {
            if should_succeed {
                println!("✓ Successfully resolved: {}", path_str);
            } else {
                println!("✗ Unexpected success for: {}", path_str);
            }
        }
        None => {
            if should_succeed {
                println!("✗ Failed to resolve: {} (expected success)", path_str);
            } else {
                println!("✓ Correctly failed to resolve: {} (as expected)", path_str);
            }
        }
    }
}
