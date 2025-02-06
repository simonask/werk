use criterion::{black_box, criterion_group, criterion_main, Criterion};
use tests::mock_io::Test;

pub fn parse_c_example(c: &mut Criterion) {
    static SOURCE: &str = include_str!("../../examples/c/Werkfile");
    c.bench_function("parse c example", |b| {
        b.iter(|| {
            black_box(werk_parser::parse_werk(
                std::path::Path::new("../../examples/c/Werkfile"),
                SOURCE,
            ))
            .unwrap()
        })
    });
}

pub fn eval_1000_lets(c: &mut Criterion) {
    let mut source = String::from("let a0 = \"1\"\n");
    for i in 1..=1000 {
        source.push_str(&format!("let a{} = a{}\n", i, i - 1));
    }
    let test = Test::new(&source).unwrap();
    c.bench_function("eval 1000 lets", |b| {
        b.iter(|| black_box(test.create_workspace(&[])).unwrap())
    });
}

pub fn parse_1000_lets(c: &mut Criterion) {
    let mut source = String::from("let a0 = \"1\"\n");
    for i in 1..=1000 {
        source.push_str(&format!("let a{} = a{}\n", i, i - 1));
    }
    c.bench_function("parse 1000 lets", |b| {
        b.iter(|| {
            black_box(werk_parser::parse_werk(
                std::path::Path::new("INPUT"),
                &source,
            ))
            .unwrap()
        })
    });
}

pub fn match_1000_arms(c: &mut Criterion) {
    let mut source = String::from("let input = \"1\"\nlet output = input | match {");
    for i in 1..=1000 {
        source.push_str(&format!("\"{}\" => \"message {}\"\n", i, i));
    }
    source.push_str("}\n");
    let test = Test::new(&source).unwrap();
    c.bench_function("match 1000 arms", |b| {
        b.iter(|| black_box(test.create_workspace(&[])).unwrap())
    });
}

criterion_group!(
    bench_eval,
    eval_1000_lets,
    parse_1000_lets,
    parse_c_example,
    match_1000_arms
);
criterion_main!(bench_eval);
