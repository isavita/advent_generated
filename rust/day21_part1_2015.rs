
use std::fs;

struct Item {
    cost: i32,
    damage: i32,
    armor: i32,
}

struct Character {
    hit_points: i32,
    damage: i32,
    armor: i32,
}

fn main() {
    let data = fs::read_to_string("input.txt").expect("Unable to read file");
    let lines: Vec<&str> = data.trim().split("\n").collect();

    let boss = Character {
        hit_points: parse_stat(lines[0]),
        damage: parse_stat(lines[1]),
        armor: parse_stat(lines[2]),
    };

    let weapons = vec![
        Item { cost: 8, damage: 4, armor: 0 },
        Item { cost: 10, damage: 5, armor: 0 },
        Item { cost: 25, damage: 6, armor: 0 },
        Item { cost: 40, damage: 7, armor: 0 },
        Item { cost: 74, damage: 8, armor: 0 },
    ];

    let armors = vec![
        Item { cost: 0, damage: 0, armor: 0 },
        Item { cost: 13, damage: 0, armor: 1 },
        Item { cost: 31, damage: 0, armor: 2 },
        Item { cost: 53, damage: 0, armor: 3 },
        Item { cost: 75, damage: 0, armor: 4 },
        Item { cost: 102, damage: 0, armor: 5 },
    ];

    let rings = vec![
        Item { cost: 0, damage: 0, armor: 0 },
        Item { cost: 25, damage: 1, armor: 0 },
        Item { cost: 50, damage: 2, armor: 0 },
        Item { cost: 100, damage: 3, armor: 0 },
        Item { cost: 20, damage: 0, armor: 1 },
        Item { cost: 40, damage: 0, armor: 2 },
        Item { cost: 80, damage: 0, armor: 3 },
    ];

    let mut min_cost = i32::MAX;
    for w in &weapons {
        for a in &armors {
            for ri in 0..rings.len() {
                for rj in ri + 1..rings.len() {
                    let mut player = Character {
                        hit_points: 100,
                        damage: w.damage + rings[ri].damage + rings[rj].damage,
                        armor: a.armor + rings[ri].armor + rings[rj].armor,
                    };
                    let cost = w.cost + a.cost + rings[ri].cost + rings[rj].cost;
                    if player_wins(&player, &boss) && cost < min_cost {
                        min_cost = cost;
                    }
                }
            }
        }
    }

    println!("{}", min_cost);
}

fn parse_stat(line: &str) -> i32 {
    let parts: Vec<&str> = line.split(": ").collect();
    parts[1].parse().unwrap()
}

fn player_wins(player: &Character, boss: &Character) -> bool {
    let player_damage = max(1, player.damage - boss.armor);
    let boss_damage = max(1, boss.damage - player.armor);

    let player_turns = (boss.hit_points + player_damage - 1) / player_damage;
    let boss_turns = (player.hit_points + boss_damage - 1) / boss_damage;

    player_turns <= boss_turns
}

fn max(a: i32, b: i32) -> i32 {
    if a > b {
        a
    } else {
        b
    }
}
