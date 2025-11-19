package tree_sitter_prime_lang_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_prime_lang "github.com/asoldo/prime_lang_treesitter/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_prime_lang.Language())
	if language == nil {
		t.Errorf("Error loading PrimeLang grammar")
	}
}
