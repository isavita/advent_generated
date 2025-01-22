
use std::fs::File;
use std::io::{self, BufRead};

const ITERATIONS: usize = 50;
const EXPAND_BY: usize = 1;

fn main() -> io::Result<()> {
    let (algorithm, mut image) = read_input("input.txt")?;
    for i in 0..ITERATIONS {
        image = enhance_image(&algorithm, &image, i % 2 == 1 && algorithm.chars().next() == Some('#'));
    }
    println!("{}", count_lit_pixels(&image));
    Ok(())
}

fn read_input(filename: &str) -> io::Result<(String, Vec<Vec<bool>>)> {
    let file = File::open(filename)?;
    let mut lines = io::BufReader::new(file).lines();
    let algorithm = lines.next().unwrap()?;
    lines.next();
    let mut image = Vec::new();
    for line in lines {
        let line = line?;
        let row: Vec<bool> = line.trim().chars().map(|c| c == '#').collect();
        image.push(row);
    }
    Ok((algorithm, image))
}

fn enhance_image(algorithm: &str, image: &Vec<Vec<bool>>, use_infinite_lit: bool) -> Vec<Vec<bool>> {
    let height = image.len();
    let width = if height > 0 { image[0].len() } else { 0 };
    let new_height = height + EXPAND_BY * 2;
    let new_width = width + EXPAND_BY * 2;
    let mut new_image = vec![vec![false; new_width]; new_height];

    for y in -(EXPAND_BY as i32)..(height as i32 + EXPAND_BY as i32) {
        for x in -(EXPAND_BY as i32)..(width as i32 + EXPAND_BY as i32) {
            let mut index: usize = 0;
            for dy in -1..=1 {
                for dx in -1..=1 {
                   index <<= 1;
                    let ny = y + dy;
                    let nx = x + dx;

                    if ny >= 0 && ny < height as i32 && nx >= 0 && nx < width as i32{
                        if image[ny as usize][nx as usize]{
                            index |= 1;
                        }
                    }else if use_infinite_lit{
                        index |= 1;
                    }
                }
            }
           
            new_image[(y + EXPAND_BY as i32) as usize][(x + EXPAND_BY as i32) as usize] = algorithm.chars().nth(index) == Some('#');
        }
    }
    new_image
}

fn count_lit_pixels(image: &Vec<Vec<bool>>) -> usize {
    image.iter().flatten().filter(|&pixel| *pixel).count()
}
