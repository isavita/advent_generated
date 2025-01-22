
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let (algorithm, image) = read_input("input.txt")?;
    let image = enhance_image(image, &algorithm, 2);
    println!("{}", count_lit_pixels(&image));
    Ok(())
}

fn read_input(filename: &str) -> io::Result<(String, Vec<Vec<u8>>)> {
    let file = File::open(filename)?;
    let mut lines = io::BufReader::new(file).lines();
    let algorithm = lines.next().unwrap()?.trim().to_string();
    lines.next();
    let image = lines
        .map(|line| line.unwrap().bytes().collect())
        .collect();
    Ok((algorithm, image))
}

fn enhance_image(mut image: Vec<Vec<u8>>, algorithm: &str, times: usize) -> Vec<Vec<u8>> {
    for i in 0..times {
        image = apply_algorithm(image, algorithm, i % 2 == 1 && algorithm.as_bytes()[0] == b'#');
    }
    image
}

fn apply_algorithm(image: Vec<Vec<u8>>, algorithm: &str, flip: bool) -> Vec<Vec<u8>> {
    let rows = image.len();
    let cols = if rows > 0 { image[0].len() } else { 0 };
    let mut enhanced_image = vec![vec![0; cols + 2]; rows + 2];

    for i in 0..rows + 2 {
        for j in 0..cols + 2 {
            let index = calculate_index(i as isize - 1, j as isize - 1, &image, flip);
            enhanced_image[i][j] = algorithm.as_bytes()[index];
        }
    }
    enhanced_image
}

fn calculate_index(i: isize, j: isize, image: &Vec<Vec<u8>>, flip: bool) -> usize {
    let mut index = 0;
     let rows = image.len() as isize;
    let cols = if rows > 0 { image[0].len() as isize } else { 0 };


    for di in -1..=1 {
        for dj in -1..=1 {
            index <<= 1;
            let ni = i + di;
            let nj = j + dj;

            if ni >= 0 && ni < rows && nj >= 0 && nj < cols {
                if image[ni as usize][nj as usize] == b'#' {
                    index |= 1;
                }
            } else if flip {
                index |= 1;
            }
        }
    }
    index
}


fn count_lit_pixels(image: &Vec<Vec<u8>>) -> usize {
    image
        .iter()
        .flatten()
        .filter(|&pixel| *pixel == b'#')
        .count()
}
