
fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let seats: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();
    
    let mut seats_copy = seats.clone();
    let mut changed = true;
    
    while changed {
        changed = false;
        let mut new_seats = seats_copy.clone();
        
        for i in 0..seats_copy.len() {
            for j in 0..seats_copy[i].len() {
                let mut occupied = 0;
                
                for x in -1..=1 {
                    for y in -1..=1 {
                        if x == 0 && y == 0 {
                            continue;
                        }
                        
                        let mut ii = i as i32 + x;
                        let mut jj = j as i32 + y;
                        
                        while ii >= 0 && ii < seats_copy.len() as i32 && jj >= 0 && jj < seats_copy[i].len() as i32 {
                            if seats_copy[ii as usize][jj as usize] == '#' {
                                occupied += 1;
                                break;
                            } else if seats_copy[ii as usize][jj as usize] == 'L' {
                                break;
                            }
                            
                            ii += x;
                            jj += y;
                        }
                    }
                }
                
                if seats_copy[i][j] == 'L' && occupied == 0 {
                    new_seats[i][j] = '#';
                    changed = true;
                } else if seats_copy[i][j] == '#' && occupied >= 5 {
                    new_seats[i][j] = 'L';
                    changed = true;
                }
            }
        }
        
        seats_copy = new_seats;
    }
    
    let occupied_seats: usize = seats_copy.iter().map(|row| row.iter().filter(|&c| *c == '#').count()).sum();
    
    println!("{}", occupied_seats);
}
