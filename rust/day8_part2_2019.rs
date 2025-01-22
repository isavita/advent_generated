
use std::fs::File;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut file = File::open("input.txt")?;
    let mut image_data = String::new();
    file.read_to_string(&mut image_data)?;
    let image_data = image_data.trim();

    let width = 25;
    let height = 6;
    let layer_size = width * height;
    let mut final_image: Vec<u8> = vec![b'2'; layer_size];

    for layer in image_data.as_bytes().chunks(layer_size) {
        for (j, &pixel) in layer.iter().enumerate() {
            if final_image[j] == b'2' {
                final_image[j] = pixel;
            }
        }
    }

    println!("Decoded image:");
    for i in 0..height {
        for j in 0..width {
            let pixel = final_image[i * width + j];
            if pixel == b'0' {
                print!(" ");
            } else if pixel == b'1' {
                print!("#");
            }
        }
        println!();
    }
    Ok(())
}
