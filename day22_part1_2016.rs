
fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = input.trim().lines().collect();

    let mut viable_pairs = 0;

    for i in 2..lines.len() {
        let parts: Vec<&str> = lines[i].split_whitespace().collect();
        let used = parts[2].trim_end_matches('T').parse::<i32>().unwrap();
        let avail = parts[3].trim_end_matches('T').parse::<i32>().unwrap();

        if used != 0 {
            for j in 2..lines.len() {
                if i != j {
                    let parts2: Vec<&str> = lines[j].split_whitespace().collect();
                    let avail2 = parts2[3].trim_end_matches('T').parse::<i32>().unwrap();

                    if used <= avail2 {
                        viable_pairs += 1;
                    }
                }
            }
        }
    }

    println!("{}", viable_pairs);
}
