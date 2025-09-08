
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Kind {
    Space,
    Elf,
    Goblin,
    Wall,
}

#[derive(Debug, Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
}

#[derive(Debug, Clone)]
struct Unit {
    id: usize,
    kind: Kind,
    hp: i32,
    power: i32,
    x: i32,
    y: i32,
    alive: bool,
}

#[derive(Debug, Clone)]
struct State {
    map: Vec<Vec<Kind>>,
    units: Vec<Unit>,
    rows: i32,
    cols: i32,
}

const OFFSETS: [Point; 4] = [
    Point { x: 0, y: -1 },
    Point { x: -1, y: 0 },
    Point { x: 1, y: 0 },
    Point { x: 0, y: 1 },
];

fn parse_map(input: &str, elf_power: i32) -> State {
    let lines: Vec<_> = input.lines().collect();
    let rows = lines.len();
    let cols = lines[0].len();
    let mut map = vec![vec![Kind::Space; cols]; rows];
    let mut units = Vec::new();
    let mut unit_id = 0;

    for (y, line) in lines.iter().enumerate() {
        for (x, c) in line.chars().enumerate() {
            let kind = match c {
                '#' => Kind::Wall,
                '.' => Kind::Space,
                'E' => Kind::Elf,
                'G' => Kind::Goblin,
                _ => Kind::Space,
            };
            map[y][x] = kind;
            if kind == Kind::Elf || kind == Kind::Goblin {
                units.push(Unit {
                    id: unit_id,
                    kind,
                    hp: 200,
                    power: if kind == Kind::Elf { elf_power } else { 3 },
                    x: x as i32,
                    y: y as i32,
                    alive: true,
                });
                unit_id += 1;
            }
        }
    }

    State {
        map,
        units,
        rows: rows as i32,
        cols: cols as i32,
    }
}

fn get_status(units: &[Unit]) -> (bool, i32) {
    let mut elves = false;
    let mut goblins = false;
    let mut total_hp = 0;

    for unit in units {
        if unit.alive {
            total_hp += unit.hp;
            if unit.kind == Kind::Elf {
                elves = true;
            } else {
                goblins = true;
            }
        }
    }

    (elves && goblins, total_hp)
}

fn bfs(start: Point, map: &[Vec<Kind>], rows: i32, cols: i32) -> (Vec<Vec<i32>>, Vec<Vec<Point>>) {
    let mut dist = vec![vec![-1; cols as usize]; rows as usize];
    let mut path = vec![vec![Point { x: -1, y: -1 }; cols as usize]; rows as usize];
    let mut queue = VecDeque::new();

    dist[start.y as usize][start.x as usize] = 0;
    queue.push_back(start);

    while let Some(curr) = queue.pop_front() {
        for offset in &OFFSETS {
            let next = Point {
                x: curr.x + offset.x,
                y: curr.y + offset.y,
            };

            if next.x >= 0 && next.x < cols && next.y >= 0 && next.y < rows {
                let next_y = next.y as usize;
                let next_x = next.x as usize;
                if map[next_y][next_x] == Kind::Space && dist[next_y][next_x] == -1 {
                    dist[next_y][next_x] = dist[curr.y as usize][curr.x as usize] + 1;
                    path[next_y][next_x] = curr;
                    queue.push_back(next);
                }
            }
        }
    }

    (dist, path)
}

fn tick(state: &mut State, stop_on_elf_death: bool) -> (bool, bool) {
    state.units.sort_by(|a, b| {
        a.y.cmp(&b.y)
            .then_with(|| a.x.cmp(&b.x))
    });

    let mut elf_died = false;
    let mut i = 0;
    while i < state.units.len() {
        if !state.units[i].alive {
            i += 1;
            continue;
        }

        let current_unit = state.units[i].clone();
        let target_kind = if current_unit.kind == Kind::Elf {
            Kind::Goblin
        } else {
            Kind::Elf
        };

        let targets_exist = state
            .units
            .iter()
            .any(|u| u.alive && u.kind == target_kind);
        if !targets_exist {
            return (false, elf_died);
        }

        let mut attack_target: Option<usize> = None;
        let mut min_hp = i32::MAX;

        for offset in &OFFSETS {
            let nx = current_unit.x + offset.x;
            let ny = current_unit.y + offset.y;
            if nx >= 0 && nx < state.cols && ny >= 0 && ny < state.rows {
                for (j, unit) in state.units.iter().enumerate() {
                    if unit.alive
                        && unit.kind == target_kind
                        && unit.x == nx
                        && unit.y == ny
                        && unit.hp < min_hp
                    {
                        min_hp = unit.hp;
                        attack_target = Some(j);
                    }
                }
            }
        }

        if attack_target.is_none() {
            let (dist, path) = bfs(
                Point {
                    x: current_unit.x,
                    y: current_unit.y,
                },
                &state.map,
                state.rows,
                state.cols,
            );

            let mut targets = Vec::new();
            let mut min_dist = i32::MAX;

            for unit in &state.units {
                if unit.alive && unit.kind == target_kind {
                    for offset in &OFFSETS {
                        let nx = unit.x + offset.x;
                        let ny = unit.y + offset.y;
                        if nx >= 0 && nx < state.cols && ny >= 0 && ny < state.rows {
                            let ny_usize = ny as usize;
                            let nx_usize = nx as usize;
                            if state.map[ny_usize][nx_usize] == Kind::Space
                                && dist[ny_usize][nx_usize] != -1
                            {
                                if dist[ny_usize][nx_usize] < min_dist {
                                    min_dist = dist[ny_usize][nx_usize];
                                    targets.clear();
                                    targets.push(Point { x: nx, y: ny });
                                } else if dist[ny_usize][nx_usize] == min_dist {
                                    targets.push(Point { x: nx, y: ny });
                                }
                            }
                        }
                    }
                }
            }

            if !targets.is_empty() {
                targets.sort_by(|a, b| a.y.cmp(&b.y).then_with(|| a.x.cmp(&b.x)));
                let chosen_target = targets[0];
                let mut current_step = chosen_target;

                while dist[current_step.y as usize][current_step.x as usize] > 1 {
                    current_step = path[current_step.y as usize][current_step.x as usize];
                    if current_step.x == -1 {
                        break;
                    }
                }

                if current_step.x != -1 && dist[current_step.y as usize][current_step.x as usize] == 1
                {
                    state.map[current_unit.y as usize][current_unit.x as usize] = Kind::Space;
                    state.units[i].x = current_step.x;
                    state.units[i].y = current_step.y;
                    state.map[current_step.y as usize][current_step.x as usize] = current_unit.kind;

                    for offset in &OFFSETS {
                        let nx = state.units[i].x + offset.x;
                        let ny = state.units[i].y + offset.y;
                        if nx >= 0 && nx < state.cols && ny >= 0 && ny < state.rows {
                            for (j, unit) in state.units.iter().enumerate() {
                                if unit.alive
                                    && unit.kind == target_kind
                                    && unit.x == nx
                                    && unit.y == ny
                                    && unit.hp < min_hp
                                {
                                    min_hp = unit.hp;
                                    attack_target = Some(j);
                                }
                            }
                        }
                    }
                }
            }
        }

        if let Some(target_idx) = attack_target {
            state.units[target_idx].hp -= state.units[i].power;
            if state.units[target_idx].hp <= 0 {
                state.units[target_idx].alive = false;
                state.map[state.units[target_idx].y as usize][state.units[target_idx].x as usize] =
                    Kind::Space;
                if state.units[target_idx].kind == Kind::Elf && stop_on_elf_death {
                    elf_died = true;
                    return (true, elf_died);
                }
            }
        }

        i += 1;
    }

    state.units.retain(|u| u.alive);
    (true, elf_died)
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);
    let input = reader
        .lines()
        .map(|l| l.unwrap())
        .collect::<Vec<_>>()
        .join("\n");

    let mut elf_power = 3;
    let mut final_outcome = 0;

    loop {
        elf_power += 1;
        let mut state = parse_map(&input, elf_power);
        let mut rounds = 0;
        let mut elf_died = false;

        loop {
            let (combat_continues, hp_sum) = get_status(&state.units);
            if !combat_continues {
                final_outcome = rounds * hp_sum;
                break;
            }

            let (round_completed, died) = tick(&mut state, true);
            elf_died = died;

            if elf_died {
                break;
            }

            if !round_completed {
                let (_, hp_sum) = get_status(&state.units);
                final_outcome = rounds * hp_sum;
                break;
            }

            rounds += 1;
        }

        if !elf_died {
            break;
        }
    }

    println!("{}", final_outcome);
}
