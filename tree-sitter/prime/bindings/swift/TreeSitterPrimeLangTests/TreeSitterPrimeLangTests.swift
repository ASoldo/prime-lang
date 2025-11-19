import XCTest
import SwiftTreeSitter
import TreeSitterPrimeLang

final class TreeSitterPrimeLangTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_prime_lang())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading PrimeLang grammar")
    }
}
