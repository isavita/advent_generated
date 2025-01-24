
use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: usize,
    y: usize,
}

fn main() {
    let f = File::open("input.txt").unwrap();
    let reader = BufReader::new(f);
    let grid: Vec<String> = reader.lines().map(|line| line.unwrap()).collect();

    let h = grid.len();
    let w = grid[0].len();
    let mut s = Point { x: 0, y: 0 };
    let mut e = Point { x: 0, y: 0 };
    let mut walls = vec![vec![false; w]; h];
    let mut track_cells = Vec::new();

    for i in 0..h {
        for j in 0..w {
            let ch = grid[i].as_bytes()[j] as char;
            if ch == 'S' {
                s = Point { x: i, y: j };
            } else if ch == 'E' {
                e = Point { x: i, y: j };
            }
            if ch == '#' {
                walls[i][j] = true;
            } else {
                track_cells.push(Point { x: i, y: j });
            }
        }
    }

    let dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)];
    let is_track = |x: usize, y: usize| x < h && y < w && !walls[x][y];

    let normal_dist_from = |start: Point| {
        let mut dist = vec![vec![-1; w]; h];
        dist[start.x][start.y] = 0;
        let mut q = VecDeque::new();
        q.push_back(start);
        while let Some(cur) = q.pop_front() {
            for &(dx, dy) in &dirs {
                let nx = cur.x as isize + dx;
                let ny = cur.y as isize + dy;
                if nx >= 0 && nx < h as isize && ny >= 0 && ny < w as isize {
                    let nx = nx as usize;
                    let ny = ny as usize;
                    if !walls[nx][ny] && dist[nx][ny] < 0 {
                        dist[nx][ny] = dist[cur.x][cur.y] + 1;
                        q.push_back(Point { x: nx, y: ny });
                    }
                }
            }
        }
        dist
    };

    let dist_from_s = normal_dist_from(s);
    let dist_from_e = normal_dist_from(e);
    let normal_cost = if dist_from_s[e.x][e.y] < 0 {
        println!("0");
        return;
    } else {
        dist_from_s[e.x][e.y]
    };

    #[derive(PartialEq, Eq, Hash)]
    struct Pair {
        sx: usize,
        sy: usize,
        ex: usize,
        ey: usize,
    }
    let mut cheats: HashMap<Pair, i32> = HashMap::new();

    for &start_pos in &track_cells {
        let sd = dist_from_s[start_pos.x][start_pos.y];
        if sd < 0 {
            continue;
        }

        let mut dist_c = vec![vec![-1; w]; h];
        dist_c[start_pos.x][start_pos.y] = 0;
        let mut q = VecDeque::new();
        q.push_back(start_pos);

        while let Some(cur) = q.pop_front() {
            let steps = dist_c[cur.x][cur.y];
            if steps == 20 {
                continue;
            }
            for &(dx, dy) in &dirs {
                let nx = cur.x as isize + dx;
                let ny = cur.y as isize + dy;
                if nx >= 0 && nx < h as isize && ny >= 0 && ny < w as isize {
                    let nx = nx as usize;
                    let ny = ny as usize;
                    if dist_c[nx][ny] < 0 {
                        dist_c[nx][ny] = steps + 1;
                        q.push_back(Point { x: nx, y: ny });
                    }
                }
            }
        }

        for x in 0..h {
            for y in 0..w {
                let s_val = dist_c[x][y];
                if s_val > 0 && s_val <= 20 && is_track(x, y) {
                    let ed = dist_from_e[x][y];
                    if ed < 0 {
                        continue;
                    }
                    let cost = sd + s_val + ed;
                    if cost < normal_cost {
                        let key = Pair {
                            sx: start_pos.x,
                            sy: start_pos.y,
                            ex: x,
                            ey: y,
                        };
                        if let Some(&old) = cheats.get(&key) {
                            if cost < old {
                                cheats.insert(key, cost);
                            }
                        } else {
                            cheats.insert(key, cost);
                        }
                    }
                }
            }
        }
    }

    let count = cheats.values().filter(|&&cost| normal_cost - cost >= 100).count();
    println!("{}", count);
}
