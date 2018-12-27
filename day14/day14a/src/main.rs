use day14a::*;

use std::io;

fn main() {
    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .expect("Failed to read input");

    let scores = get_next_ten_scores(
        input
            .trim()
            .parse()
            .expect("Input must be a positive integer"),
    );

    println!(
        "{}",
        scores.iter().map(ToString::to_string).collect::<String>()
    )
}
