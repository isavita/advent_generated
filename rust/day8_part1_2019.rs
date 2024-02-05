
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let image: Vec<u32> = input.trim().chars().map(|c| c.to_digit(10).unwrap()).collect();
    
    let width = 25;
    let height = 6;
    let layer_size = width * height;
    let num_layers = image.len() / layer_size;

    let mut min_zeros = std::u32::MAX;
    let mut result = 0;

    for i in 0..num_layers {
        let layer = &image[i * layer_size..(i + 1) * layer_size];
        let num_zeros = layer.iter().filter(|&&x| x == 0).count() as u32;
        if num_zeros < min_zeros {
            min_zeros = num_zeros;
            result = (layer.iter().filter(|&&x| x == 1).count() as u32) * (layer.iter().filter(|&&x| x == 2).count() as u32);
        }
    }

    println!("{}", result);
}
