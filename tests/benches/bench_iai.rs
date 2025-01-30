fn iai_bench_parse() {
    static SOURCE: &str = include_str!("../../examples/c/Werkfile");
    iai::black_box(werk_parser::parse_werk(SOURCE)).unwrap();
}

iai::main!(iai_bench_parse);
