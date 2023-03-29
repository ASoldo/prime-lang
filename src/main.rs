mod compiler;
mod interpreter;
mod parser;
use compiler::Compiler;
use inkwell::context::Context;
use interpreter::interpret;
use parser::tokenize;
use std::env;
use std::fs;
use std::process::Command;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 3 {
        eprintln!("Usage: ./prime-lang run <filename.prime>");
        std::process::exit(1);
    }

    if args[1] != "run" {
        eprintln!("Invalid command. Usage: ./prime-lang run <filename.prime>");
        std::process::exit(1);
    }

    let filename = &args[2];
    if !filename.ends_with(".prime") {
        eprintln!("Invalid file extension. Only .prime files are allowed.");
        std::process::exit(1);
    }

    let content = fs::read_to_string(filename).expect("Could not read the file");
    let tokens = tokenize(&content);
    // interpret(tokens);
    println!("Tokens: {:?}", tokens);

    // Initialize LLVM context and create the compiler instance
    let context = Context::create();
    let mut compiler = Compiler::new(&context);

    // Compile the code
    compiler.compile(&tokens);
    compiler.print_ir();
    compiler
        .write_ir_to_file("output.ll")
        .expect("Failed to write LLVM IR to file");

    // Call llc
    let output = Command::new("llc")
        .arg("-relocation-model=pic")
        .arg("-filetype=obj")
        .arg("output.ll")
        .arg("-o")
        .arg("output.o")
        .output()
        .expect("Failed to execute llc");
    println!("llc stdout: {}", String::from_utf8_lossy(&output.stdout));
    println!("llc stderr: {}", String::from_utf8_lossy(&output.stderr));

    // Call gcc
    let output = Command::new("gcc")
        .arg("output.o")
        .arg("-o")
        .arg("output")
        .output()
        .expect("Failed to execute gcc");
    println!("gcc stdout: {}", String::from_utf8_lossy(&output.stdout));
    println!("gcc stderr: {}", String::from_utf8_lossy(&output.stderr));
}
