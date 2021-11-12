/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::fs;

use criterion::{criterion_group, criterion_main, Criterion};
use kaleidoscope_parser::{
    error::Error,
    located::{LocatedSlice, Position},
    parse,
};

fn benchmark_kal(c: &mut Criterion) {
    println!("{}", std::env::current_dir().unwrap().display());
    let file = fs::read_to_string("../benchmark.kal").unwrap();
    c.bench_function("[nom] benchmark.kal", |b| {
        b.iter(|| {
            let input = LocatedSlice::new(file.as_str());
            parse::<_, Error<_>, Position>(input)
        })
    });
}

criterion_group!(benches, benchmark_kal);
criterion_main!(benches);
