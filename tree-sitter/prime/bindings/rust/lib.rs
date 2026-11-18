//! This crate provides PrimeLang language support for the [tree-sitter] parsing library.
//!
//! Typically, you will use the [`LANGUAGE`] constant to add this language to a
//! tree-sitter [`Parser`], and then use the parser to parse some code:
//!
//! ```
//! let code = "module demo; fn main() { () }";
//! let mut parser = tree_sitter::Parser::new();
//! let language = tree_sitter_prime_lang::LANGUAGE;
//! parser
//!     .set_language(&language.into())
//!     .expect("Error loading PrimeLang parser");
//! let tree = parser.parse(code, None).unwrap();
//! assert!(!tree.root_node().has_error());
//! ```
//!
//! [`Parser`]: https://docs.rs/tree-sitter/0.25.10/tree_sitter/struct.Parser.html
//! [tree-sitter]: https://tree-sitter.github.io/

use tree_sitter_language::LanguageFn;

extern "C" {
    fn tree_sitter_prime() -> *const ();
}

/// The tree-sitter [`LanguageFn`] for this grammar.
pub const LANGUAGE: LanguageFn = unsafe { LanguageFn::from_raw(tree_sitter_prime) };

/// The content of the [`node-types.json`] file for this grammar.
///
/// [`node-types.json`]: https://tree-sitter.github.io/tree-sitter/using-parsers/6-static-node-types
pub const NODE_TYPES: &str = include_str!("../../src/node-types.json");

// NOTE: uncomment these to include any queries that this grammar contains:

// pub const HIGHLIGHTS_QUERY: &str = include_str!("../../queries/highlights.scm");
// pub const INJECTIONS_QUERY: &str = include_str!("../../queries/injections.scm");
// pub const LOCALS_QUERY: &str = include_str!("../../queries/locals.scm");
// pub const TAGS_QUERY: &str = include_str!("../../queries/tags.scm");

#[cfg(test)]
mod tests {
    fn parse(source: &str) -> tree_sitter::Tree {
        let language: tree_sitter::Language = super::LANGUAGE.into();
        assert_eq!(language.abi_version(), 15);
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&language).unwrap();
        parser.parse(source, None).unwrap()
    }

    #[test]
    fn distinguishes_unit_type_and_expression() {
        let tree = parse("module demo; fn sample() -> () { () }");
        let root = tree.root_node();
        assert!(!root.has_error(), "{}", root.to_sexp());
        let function = root.named_child(1).unwrap().named_child(0).unwrap();
        let returns = function.child_by_field_name("returns").unwrap();
        let body = function.child_by_field_name("body").unwrap();
        assert_eq!(returns.named_child(0).unwrap().kind(), "tuple_type");
        assert_eq!(
            body.child_by_field_name("tail").unwrap().kind(),
            "tuple_expression"
        );
    }

    #[test]
    fn reports_invalid_and_incomplete_syntax() {
        for source in [
            "module demo; fn sample() { let = ; }",
            "module demo; fn sample() { ()",
        ] {
            assert!(parse(source).root_node().has_error(), "{source}");
        }
    }
}
