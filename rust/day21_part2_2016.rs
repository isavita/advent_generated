
use std::fs::File;
use std::io::{self, BufRead};
use std::str::FromStr;

struct Scrambler {
    pw: Vec<char>,
}

impl Scrambler {
    fn new(pw: &str) -> Self {
        Scrambler { pw: pw.chars().collect() }
    }

    fn to_string(&self) -> String {
        self.pw.iter().collect()
    }

    fn swap_positions(&mut self, x: usize, y: usize) {
        self.pw.swap(x, y);
    }

    fn swap_letters(&mut self, x: char, y: char) {
        let x_index = self.pw.iter().position(|&c| c == x).unwrap();
        let y_index = self.pw.iter().position(|&c| c == y).unwrap();
        self.swap_positions(x_index, y_index);
    }

    fn rotate(&mut self, steps: i32) {
        let length = self.pw.len() as i32;
        let steps = (steps % length + length) as usize % self.pw.len();
        self.pw.rotate_right(steps);
    }

    fn rotate_letter(&mut self, x: char) {
        let index = self.pw.iter().position(|&c| c == x).unwrap();
        let steps = if index >= 4 { index + 2 } else { index + 1 };
        self.rotate(steps as i32);
    }

    fn derotate_letter(&mut self, x: char) {
        let index = self.pw.iter().position(|&c| c == x).unwrap();
        let rot = if index % 2 == 1 {
             -(index as i32 + 1) / 2
        } else if index != 0 {
           (6 - index as i32) / 2
        }else {
           -1
       };
        self.rotate(rot);
    }
    

    fn reverse(&mut self, x: usize, y: usize) {
        let mut start = x;
        let mut end = y;
        while start < end {
            self.pw.swap(start, end);
            start += 1;
            end -= 1;
        }
    }

    fn move_(&mut self, x: usize, y: usize) {
        let ch = self.pw[x];
        if x < y {
            for i in x..y{
                self.pw[i]=self.pw[i+1];
            }
        } else {
            for i in (y+1..=x).rev(){
                self.pw[i]=self.pw[i-1];
            }
        }
        self.pw[y] = ch;
    }

    fn scramble(&mut self, instructions: &[String], direction: i32) {
        let mut instructions = instructions.to_vec();
        if direction < 0 {
            instructions.reverse();
        }
        for instruction in instructions {
            let line: Vec<&str> = instruction.split_whitespace().collect();
            match line[0] {
                "swap" => {
                    let x = line[2];
                    let y = line[line.len()-1];
                    if line[1] == "position" {
                        let xi = usize::from_str(x).unwrap();
                        let yi = usize::from_str(y).unwrap();
                        self.swap_positions(xi, yi);
                    } else {
                        self.swap_letters(x.chars().next().unwrap(), y.chars().next().unwrap());
                    }
                },
                "rotate" => {
                    if line[1] == "based" {
                        if direction > 0 {
                            self.rotate_letter(line[line.len()-1].chars().next().unwrap());
                        } else {
                            self.derotate_letter(line[line.len()-1].chars().next().unwrap());
                        }
                    } else {
                        let mut x = i32::from_str(line[2]).unwrap();
                        if line[1] == "left" {
                             x = -x;
                        }
                        if direction < 0 {
                            x = -x;
                        }
                         self.rotate(x);
                    }
                },
                "reverse" => {
                    let x = line[2];
                    let y = line[line.len()-1];
                     let xi = usize::from_str(x).unwrap();
                     let yi = usize::from_str(y).unwrap();
                     self.reverse(xi, yi);
                },
                "move" => {
                   let x = line[2];
                   let y = line[line.len()-1];
                    let mut xi = usize::from_str(x).unwrap();
                    let mut yi = usize::from_str(y).unwrap();
                    if direction < 0 {
                         std::mem::swap(&mut xi, &mut yi);
                    }
                    self.move_(xi, yi);
                },
                 _ => unreachable!(),
            }
        }
    }

     fn unscramble(&mut self, instructions: &[String]) {
        self.scramble(instructions, -1);
    }
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);
    let instructions: Vec<String> = reader.lines().filter_map(Result::ok).collect();

    let mut scrambler = Scrambler::new("fbgdceah");
    scrambler.unscramble(&instructions);

    println!("{}", scrambler.to_string());
    Ok(())
}
