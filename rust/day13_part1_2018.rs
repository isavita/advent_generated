
use std::{
    fs::File,
    io::{self, BufRead},
};

#[derive(Clone, Copy, PartialEq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Clone, Copy)]
struct Cart {
    x: usize,
    y: usize,
    dir: Direction,
    turn: usize,
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let mut track: Vec<Vec<char>> = Vec::new();
    let mut carts: Vec<Cart> = Vec::new();

    for (y, line) in lines.iter().enumerate() {
        let mut row = Vec::new();
        for (x, c) in line.chars().enumerate() {
            match c {
                '>' => {
                    row.push('-');
                    carts.push(Cart {
                        x,
                        y,
                        dir: Direction::Right,
                        turn: 0,
                    });
                }
                '<' => {
                    row.push('-');
                    carts.push(Cart {
                        x,
                        y,
                        dir: Direction::Left,
                        turn: 0,
                    });
                }
                '^' => {
                    row.push('|');
                    carts.push(Cart {
                        x,
                        y,
                        dir: Direction::Up,
                        turn: 0,
                    });
                }
                'v' => {
                    row.push('|');
                    carts.push(Cart {
                        x,
                        y,
                        dir: Direction::Down,
                        turn: 0,
                    });
                }
                _ => row.push(c),
            }
        }
        track.push(row);
    }
    
    let mut collision = false;

    while !collision {
        for i in 0..carts.len() {
            let cart = carts[i];
            let new_cart = match cart.dir {
                Direction::Right => move_right(&track, cart),
                Direction::Left => move_left(&track, cart),
                Direction::Up => move_up(&track, cart),
                Direction::Down => move_down(&track, cart),
            };
            carts[i] = new_cart;
        }
        
        for i in 0..carts.len(){
            for j in i+1..carts.len(){
                if carts[i].x == carts[j].x && carts[i].y == carts[j].y{
                    collision = true;
                    println!("{},{}", carts[i].x, carts[i].y);
                    break;
                }
            }
            if collision {
                break;
            }
        }
    }


    Ok(())
}

fn move_down(track: &Vec<Vec<char>>, mut cart: Cart) -> Cart {
    let next_char = track[cart.y + 1][cart.x];
    match next_char {
        '/' => cart.dir = Direction::Left,
        '\\' => cart.dir = Direction::Right,
        '+' => {
            cart.dir = match cart.turn {
                0 => Direction::Right,
                1 => cart.dir,
                2 => Direction::Left,
                _ => panic!("Invalid turn state"),
            };
            cart.turn = (cart.turn + 1) % 3;
        }
        _ => {}
    }
    cart.y += 1;
    cart
}

fn move_up(track: &Vec<Vec<char>>, mut cart: Cart) -> Cart {
    let next_char = track[cart.y - 1][cart.x];
     match next_char {
        '/' => cart.dir = Direction::Right,
        '\\' => cart.dir = Direction::Left,
        '+' => {
            cart.dir = match cart.turn {
                0 => Direction::Left,
                1 => cart.dir,
                2 => Direction::Right,
                _ => panic!("Invalid turn state"),
            };
             cart.turn = (cart.turn + 1) % 3;
        }
        _ => {}
    }
    cart.y -= 1;
    cart
}

fn move_left(track: &Vec<Vec<char>>, mut cart: Cart) -> Cart {
    let next_char = track[cart.y][cart.x - 1];
    match next_char {
        '/' => cart.dir = Direction::Down,
        '\\' => cart.dir = Direction::Up,
        '+' => {
           cart.dir = match cart.turn {
                0 => Direction::Down,
                1 => cart.dir,
                2 => Direction::Up,
                _ => panic!("Invalid turn state"),
            };
            cart.turn = (cart.turn + 1) % 3;
        }
       _ => {}
    }
    cart.x -= 1;
    cart
}

fn move_right(track: &Vec<Vec<char>>, mut cart: Cart) -> Cart {
    let next_char = track[cart.y][cart.x + 1];
     match next_char {
        '/' => cart.dir = Direction::Up,
        '\\' => cart.dir = Direction::Down,
        '+' => {
           cart.dir = match cart.turn {
                0 => Direction::Up,
                1 => cart.dir,
                2 => Direction::Down,
                _ => panic!("Invalid turn state"),
            };
            cart.turn = (cart.turn + 1) % 3;
        }
        _ => {}
    }
    cart.x += 1;
    cart
}
