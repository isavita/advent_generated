
use std::fs;
use std::io;

fn main() -> io::Result<()> {
    let input = fs::read_to_string("input.txt")?;
    let disk_map = input.trim();

    let mut blocks = Vec::new();
    let mut file_id = 0;
    let mut is_file = true;

    let mut chars = disk_map.chars();
    while let Some(c) = chars.next() {
        let len = c.to_digit(10).unwrap() as usize;
        if is_file {
            for _ in 0..len {
                blocks.push(Some(file_id));
            }
            file_id += 1;
        } else {
            for _ in 0..len {
                blocks.push(None);
            }
        }
        is_file = !is_file;
    }

    // Compact
    let mut next_free = 0;
    let mut next_file_rev = blocks.len() - 1;

    while next_free < next_file_rev {
        
       while next_free < blocks.len() && blocks[next_free].is_some(){
            next_free += 1;
       }
        while next_file_rev >= 0 && blocks[next_file_rev].is_none() {
            next_file_rev -= 1;
        }
        if next_free < next_file_rev {
             blocks[next_free] = blocks[next_file_rev].take();
        }

    }

    // Calculate checksum
    let checksum: usize = blocks
        .iter()
        .enumerate()
        .filter_map(|(i, &block)| block.map(|id| i * id))
        .sum();
    println!("{}", checksum);
    Ok(())
}
