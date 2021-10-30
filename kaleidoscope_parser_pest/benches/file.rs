use std::fs;

use criterion::{criterion_group, criterion_main, Criterion};
use kaleidoscope_parser_pest::{located::Position, parse, KaleidoscopeParser, Rule};
use pest::Parser;

fn benchmark_kal(c: &mut Criterion) {
    println!("{}", std::env::current_dir().unwrap().display());
    let file = fs::read_to_string("../benchmark.kal").unwrap();
    c.bench_function("[pest] benchmark.kal", |b| {
        b.iter(|| parse::<Position>(file.as_str()))
    });
    c.bench_function("[pest::parse] benchmark.kal", |b| {
        b.iter(|| KaleidoscopeParser::parse(Rule::file, file.as_str()))
    });
}

criterion_group!(benches, benchmark_kal);
criterion_main!(benches);
