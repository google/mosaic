fn main() -> Result<(), Box<dyn std::error::Error>> {
    clang_sys::load().expect("Couldn't find libclang");
    std::process::exit(peasy::main()?)
}
