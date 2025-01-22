
use std::cmp::Ordering;
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug, Clone)]
struct Cart {
    x: usize,
    y: usize,
    direction: char,
    turns: usize,
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);

    let mut tracks: Vec<Vec<char>> = Vec::new();
    let mut carts: Vec<Cart> = Vec::new();

    for (y, line) in reader.lines().enumerate() {
        let line = line.unwrap();
        let mut track_line: Vec<char> = Vec::with_capacity(line.len());
        for (x, r) in line.chars().enumerate() {
            match r {
                '>' | '<' | '^' | 'v' => {
                    carts.push(Cart {
                        x,
                        y,
                        direction: r,
                        turns: 0,
                    });
                    track_line.push(if r == '>' || r == '<' { '-' } else { '|' });
                }
                _ => track_line.push(r),
            }
        }
        tracks.push(track_line);
    }
    while carts.len() > 1 {
        carts.sort_by(|a, b| {
            if a.y == b.y {
                a.x.cmp(&b.x)
            } else {
                a.y.cmp(&b.y)
            }
        });

        let mut to_remove: HashSet<usize> = HashSet::new();

        for i in 0..carts.len() {
            if to_remove.contains(&i) {
                continue;
            }
            move_cart(&mut carts[i], &tracks);

            if let Some(crash_index) = check_crash(&carts[i], &carts) {
                for (idx, cart) in carts.iter().enumerate() {
                    if cart.x == carts[i].x && cart.y == carts[i].y && idx != i{
                        to_remove.insert(i);
                        to_remove.insert(idx);
                        break
                    }
                }
            }
        }
        let mut new_carts = Vec::new();
        for (i, cart) in carts.iter().enumerate() {
            if !to_remove.contains(&i){
                new_carts.push(cart.clone())
            }
        }
        carts = new_carts
    }
    println!("{},{}", carts[0].x, carts[0].y);
}

fn move_cart(cart: &mut Cart, tracks: &Vec<Vec<char>>) {
    match cart.direction {
        '>' => cart.x += 1,
        '<' => cart.x -= 1,
        '^' => cart.y -= 1,
        'v' => cart.y += 1,
        _ => (),
    }

    match tracks[cart.y][cart.x] {
        '+' => turn_cart(cart),
        '/' | '\\' => change_direction(cart, tracks[cart.y][cart.x]),
        _ => (),
    }
}

fn turn_cart(cart: &mut Cart) {
    cart.direction = match cart.turns % 3 {
        0 => match cart.direction {
            '>' => '^',
            '<' => 'v',
            '^' => '<',
            'v' => '>',
            _ => cart.direction,
        },
        2 => match cart.direction {
            '>' => 'v',
            '<' => '^',
            '^' => '>',
            'v' => '<',
            _ => cart.direction,
        },
        _ => cart.direction,
    };
    cart.turns += 1;
}

fn change_direction(cart: &mut Cart, track: char) {
    cart.direction = match track {
        '/' => match cart.direction {
            '>' => '^',
            '<' => 'v',
            '^' => '>',
            'v' => '<',
            _ => cart.direction,
        },
        '\\' => match cart.direction {
            '>' => 'v',
            '<' => '^',
            '^' => '<',
            'v' => '>',
            _ => cart.direction,
        },
        _ => cart.direction,
    }
}

fn check_crash(cart: &Cart, carts: &Vec<Cart>) -> Option<usize> {
    for (i, c) in carts.iter().enumerate() {
        if c.x == cart.x && c.y == cart.y && i != get_index_of_cart(cart, &carts) {
           return Some(i)
        }
    }
    None
}

fn get_index_of_cart(cart: &Cart, carts: &Vec<Cart>) -> usize{
    for (i,c) in carts.iter().enumerate() {
        if c.x == cart.x && c.y == cart.y && c.direction == cart.direction && c.turns == cart.turns {
            return i
        }
    }
    0
}
