
use std::fs::File;
use std::io::{self, BufRead};
use std::str::FromStr;
use std::cmp::{max, min};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Range {
    start: i64,
    end: i64,
}

impl Range {
    fn new(start: i64, end: i64) -> Self {
        Range { start, end }
    }

    fn size(&self) -> i64 {
        if self.end < self.start {
            0
        } else {
             self.end - self.start + 1
        }
    }

    fn intersect(&self, other: &Range) -> Option<Range> {
        let start = max(self.start, other.start);
        let end = min(self.end, other.end);
        if start <= end {
            Some(Range { start, end })
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Cuboid {
    x_range: Range,
    y_range: Range,
    z_range: Range,
}

impl Cuboid {
    fn new(x_range: Range, y_range: Range, z_range: Range) -> Self {
        Cuboid {
            x_range,
            y_range,
            z_range,
        }
    }

    fn size(&self) -> i64 {
        self.x_range.size() * self.y_range.size() * self.z_range.size()
    }

    fn intersect(&self, other: &Cuboid) -> Option<Cuboid> {
        let x_intersect = self.x_range.intersect(&other.x_range);
        let y_intersect = self.y_range.intersect(&other.y_range);
        let z_intersect = self.z_range.intersect(&other.z_range);

        match (x_intersect, y_intersect, z_intersect) {
            (Some(x), Some(y), Some(z)) => Some(Cuboid {
                x_range: x,
                y_range: y,
                z_range: z,
            }),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Operation {
    On,
    Off,
}

impl FromStr for Operation {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "on" => Ok(Operation::On),
            "off" => Ok(Operation::Off),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Step {
    operation: Operation,
    cuboid: Cuboid,
}

fn parse_line(line: &str) -> Result<Step, ()> {
    let parts: Vec<&str> = line.split(' ').collect();
    if parts.len() != 2 {
        return Err(());
    }

    let operation = Operation::from_str(parts[0])?;
    let ranges_str: Vec<&str> = parts[1].split(',').collect();

    if ranges_str.len() != 3 {
        return Err(());
    }

     let mut parse_range = |s: &str| -> Result<Range, ()> {
        let range_parts: Vec<&str> = s.split('=').collect();
        if range_parts.len() != 2 {
           return Err(());
        }

        let num_parts: Vec<&str> = range_parts[1].split("..").collect();

        if num_parts.len() != 2{
            return Err(());
        }

        let start = num_parts[0].parse::<i64>().map_err(|_| ())?;
        let end = num_parts[1].parse::<i64>().map_err(|_| ())?;
        Ok(Range::new(start,end))
    };
    
    let x_range = parse_range(ranges_str[0])?;
    let y_range = parse_range(ranges_str[1])?;
    let z_range = parse_range(ranges_str[2])?;

    Ok(Step {
        operation,
        cuboid: Cuboid::new(x_range, y_range, z_range),
    })
}

fn solve(steps: &[Step]) -> i64 {
    let mut cuboids: Vec<(Cuboid, i64)> = Vec::new();

    for step in steps {
        let current_cuboid = step.cuboid;
        let mut additions: Vec<(Cuboid, i64)> = Vec::new();

        for (cuboid, value) in &cuboids {
            if let Some(intersection) = current_cuboid.intersect(cuboid) {
                additions.push((intersection, -*value));
            }
        }

        if step.operation == Operation::On {
            additions.push((current_cuboid, 1));
        }

        cuboids.extend(additions);
    }

    cuboids.iter().map(|(c, v)| c.size() * v).sum()
}


fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut steps: Vec<Step> = Vec::new();
    for line in reader.lines() {
        let line = line?;
        if let Ok(step) = parse_line(&line) {
            steps.push(step);
        } else {
            eprintln!("Failed to parse line: {}", line);
        }
    }
    
    let part1_region = Cuboid {
        x_range: Range { start: -50, end: 50 },
        y_range: Range { start: -50, end: 50 },
        z_range: Range { start: -50, end: 50 },
    };


    let part1_steps: Vec<Step> = steps
        .iter()
        .filter_map(|step| {
             if let Some(intersection) = step.cuboid.intersect(&part1_region) {
                Some(Step {
                    operation: step.operation,
                    cuboid: intersection,
                })
            } else {
                None
            }
        }).collect();
    
    let part1_result = solve(&part1_steps);
    println!("Part 1: {}", part1_result);

    let part2_result = solve(&steps);
    println!("Part 2: {}", part2_result);

    Ok(())
}
