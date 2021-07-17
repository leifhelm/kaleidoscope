#[macro_use]
extern crate clap;

// #[global_allocator]
// static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

fn main() {
    let matches = clap_app!(kaleidoscope =>
        (version: "0.1.0")
        (author: "Jakob Leifhelm")
        (about: "A demo programming language")
        (@arg FILE: +required "Sets the file to compile")
        (@arg color: --color +takes_value "Coloring: auto, always, never")
    )
    .get_matches();
    kaleidoscope::run(matches);
}
