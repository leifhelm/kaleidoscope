/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use kaleidoscope::run;

#[macro_use]
extern crate clap;

fn main() {
    let matches = clap_app!(kaleidoscope =>
        (version: "0.1.0")
        (author: "Jakob Leifhelm")
        (about: "A demo programming language")
        (@arg FILE: +required "Sets the file to compile")
        (@arg color: --color +takes_value "Coloring: auto, always, never")
    )
    .get_matches();
    run(matches);
}
