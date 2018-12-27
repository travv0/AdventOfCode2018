use day14b::*;

use std::io;

fn main() {
    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .expect("Failed to read input");

    let count = get_score_count_before(input.trim());

    println!("{}", count)
}
