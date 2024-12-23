# Taifya-course-work

This project was made as a course work for subject "Theory of automatas and formal languages".

Project - is a compiler for a programming language, and it consists of 5 elements:
* Lexical analyzer
* Syntactical analyzer
* Semantical analyzer
* Evaluator
* Compiler (hidden behind `compiler` feature)

### How to run
* Clone repository
* [Download Rust](https://rustup.rs/)
* Build project: `cargo build --release`

For `compiler` feature to work you need to:
* [Download LLVM 18.1.8](https://github.com/llvm/llvm-project/releases/tag/llvmorg-18.1.8) (clang+llvm is needed)
* Install GCC (gcc is required for final compilation step)
* Read [llvm-sys](https://docs.rs/crate/llvm-sys/180.0.0) build requirements how to make LLVM work
* Build project with `compiler` feature: `cargo build --release --features compiler`