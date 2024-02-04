
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut total_paper = 0;

    for line in input.lines() {
        let dimensions: Vec<i32> = line.split('x').map(|x| x.parse().unwrap()).collect();
        let l = dimensions[0];
        let w = dimensions[1];
        let h = dimensions[2];

        let area1 = l * w;
        let area2 = w * h;
        let area3 = h * l;
        let slack = *[area1, area2, area3].iter().min().unwrap();

        total_paper += 2 * area1 + 2 * area2 + 2 * area3 + slack;
    }

    println!("{}", total_paper);
}
