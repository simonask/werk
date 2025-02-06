fn iai_bench_parse() {
    static SOURCE: &str = include_str!("../../examples/c/Werkfile");
    iai::black_box(werk_parser::parse_werk(
        std::path::Path::new("../../examples/c/Werkfile"),
        SOURCE,
    ))
    .unwrap();
}

iai::main!(iai_bench_parse);
