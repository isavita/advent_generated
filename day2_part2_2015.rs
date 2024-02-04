use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let mut total_paper = 0;
    let mut total_ribbon = 0;

    for line in contents.lines() {
        let dimensions: Vec<i32> = line.split('x')
            .map(|x| x.parse().unwrap())
            .collect();

        let l = dimensions[0];
        let w = dimensions[1];
        let h = dimensions[2];

        let area = 2*l*w + 2*w*h + 2*h*l;
        let extra = *vec![l*w, w*h, h*l].iter().min().unwrap();

        total_paper += area + extra;

        let ribbon = 2*l + 2*w + 2*h - 2*vec![l, w, h].iter().max().unwrap();
        let bow = l * w * h;

        total_ribbon += ribbon + bow;
    }

    println!("{}", total_paper);
    println!("{}", total_ribbon);
}