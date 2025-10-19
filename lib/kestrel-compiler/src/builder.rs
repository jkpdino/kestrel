use crate::compilation::Compilation;
use std::fs;
use std::io;

/// Builder for creating a `Compilation`.
///
/// Use this to add source files from strings or file paths,
/// then call `build()` to compile all sources.
#[derive(Default)]
pub struct CompilationBuilder {
    sources: Vec<(String, String)>, // (name, content) pairs
}

impl CompilationBuilder {
    /// Create a new compilation builder.
    pub fn new() -> Self {
        Self {
            sources: Vec::new(),
        }
    }

    /// Add a source file from a string.
    ///
    /// # Arguments
    /// * `name` - The name of the file (e.g., "main.ks")
    /// * `source` - The source code content
    ///
    /// # Example
    /// ```no_run
    /// # use kestrel_compiler::CompilationBuilder;
    /// let builder = CompilationBuilder::new()
    ///     .add_source("main.ks", "module Main\nclass Foo {}");
    /// ```
    pub fn add_source(mut self, name: impl Into<String>, source: impl Into<String>) -> Self {
        self.sources.push((name.into(), source.into()));
        self
    }

    /// Add a source file from a file path.
    ///
    /// Reads the file from disk and adds it to the compilation.
    ///
    /// # Arguments
    /// * `path` - The path to the file
    ///
    /// # Errors
    /// Returns an error if the file cannot be read.
    ///
    /// # Example
    /// ```no_run
    /// # use kestrel_compiler::CompilationBuilder;
    /// let builder = CompilationBuilder::new()
    ///     .add_file("src/main.ks")
    ///     .unwrap();
    /// ```
    pub fn add_file(mut self, path: impl AsRef<std::path::Path>) -> io::Result<Self> {
        let path = path.as_ref();
        let source = fs::read_to_string(path)?;
        let name = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown")
            .to_string();
        self.sources.push((name, source));
        Ok(self)
    }

    /// Build the compilation.
    ///
    /// This performs lexing, parsing, and semantic analysis on all source files.
    /// Diagnostics are collected automatically during this process.
    ///
    /// # Example
    /// ```no_run
    /// # use kestrel_compiler::CompilationBuilder;
    /// let compilation = CompilationBuilder::new()
    ///     .add_source("main.ks", "module Main\nclass Foo {}")
    ///     .build();
    ///
    /// if compilation.has_errors() {
    ///     compilation.diagnostics().emit().unwrap();
    /// }
    /// ```
    pub fn build(self) -> Compilation {
        Compilation::from_sources(self.sources)
    }
}
