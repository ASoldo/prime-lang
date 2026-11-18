// swift-tools-version:5.9

import Foundation
import PackageDescription

var sources = ["src/parser.c"]
if FileManager.default.fileExists(atPath: "src/scanner.c") {
    sources.append("src/scanner.c")
}

let package = Package(
    name: "TreeSitterPrimeLang",
    platforms: [
        .macOS(.v10_13), .macCatalyst(.v13), .iOS(.v12),
        .tvOS(.v12), .watchOS(.v5), .visionOS(.v1),
    ],
    products: [
        .library(name: "TreeSitterPrimeLang", targets: ["TreeSitterPrimeLang"]),
    ],
    dependencies: [
        .package(url: "https://github.com/tree-sitter/swift-tree-sitter", .upToNextMinor(from: "0.25.0")),
    ],
    targets: [
        .target(
            name: "TreeSitterPrimeLang",
            dependencies: [],
            path: ".",
            sources: sources,
            publicHeadersPath: "bindings/swift",
            cSettings: [.headerSearchPath("src")]
        ),
        .testTarget(
            name: "TreeSitterPrimeLangTests",
            dependencies: [
                .product(name: "SwiftTreeSitter", package: "swift-tree-sitter"),
                "TreeSitterPrimeLang",
            ],
            path: "bindings/swift/TreeSitterPrimeLangTests"
        )
    ],
    cLanguageStandard: .c11
)
