
use std::cmp::min;
use std::collections::VecDeque;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Spell {
    MagicMissile,
    Drain,
    Shield,
    Poison,
    Recharge,
}

#[derive(Clone, Copy, Debug)]
struct Effect {
    spell: Spell,
    timer: i32,
}

#[derive(Clone, Debug)]
struct State {
    player_hp: i32,
    player_mana: i32,
    boss_hp: i32,
    boss_damage: i32,
    effects: Vec<Effect>,
    mana_spent: i32,
    hard_mode: bool,
}

impl State {
    fn new(boss_hp: i32, boss_damage: i32, hard_mode: bool) -> Self {
        State {
            player_hp: 50,
            player_mana: 500,
            boss_hp,
            boss_damage,
            effects: Vec::new(),
            mana_spent: 0,
            hard_mode,
        }
    }

    fn apply_effects(&mut self) {
        let mut new_effects = Vec::new();
        for effect in &mut self.effects {
            match effect.spell {
                Spell::Shield => {}
                Spell::Poison => self.boss_hp -= 3,
                Spell::Recharge => self.player_mana += 101,
                _ => {}
            }
            effect.timer -= 1;
            if effect.timer > 0 {
                new_effects.push(*effect);
            }
        }
        self.effects = new_effects;
    }

    fn player_turn(&mut self, spell: Spell) -> bool {
        if self.hard_mode {
            self.player_hp -= 1;
            if self.player_hp <= 0 {
                return false;
            }
        }
        self.apply_effects();

        let (mana_cost, damage, heal) = match spell {
            Spell::MagicMissile => (53, 4, 0),
            Spell::Drain => (73, 2, 2),
            Spell::Shield => (113, 0, 0),
            Spell::Poison => (173, 0, 0),
            Spell::Recharge => (229, 0, 0),
        };

        if self.player_mana < mana_cost {
            return false;
        }
        if self
            .effects
            .iter()
            .any(|e| e.spell == spell && e.timer > 0)
        {
            return false;
        }

        self.player_mana -= mana_cost;
        self.mana_spent += mana_cost;

        match spell {
            Spell::MagicMissile | Spell::Drain => {
                self.boss_hp -= damage;
                self.player_hp += heal;
            }
            Spell::Shield => self.effects.push(Effect {
                spell,
                timer: 6,
            }),
            Spell::Poison => self.effects.push(Effect {
                spell,
                timer: 6,
            }),
            Spell::Recharge => self.effects.push(Effect {
                spell,
                timer: 5,
            }),
        }
        true
    }

    fn boss_turn(&mut self) {
        self.apply_effects();
        if self.boss_hp <= 0 {
            return;
        }
        let armor = if self.effects.iter().any(|e| e.spell == Spell::Shield) {
            7
        } else {
            0
        };
        let damage = (self.boss_damage - armor).max(1);
        self.player_hp -= damage;
    }

    fn is_win(&self) -> bool {
        self.boss_hp <= 0
    }

    fn is_lose(&self) -> bool {
        self.player_hp <= 0
    }
}

fn solve(boss_hp: i32, boss_damage: i32, hard_mode: bool) -> i32 {
    let initial_state = State::new(boss_hp, boss_damage, hard_mode);
    let mut min_mana = i32::MAX;
    let mut queue = VecDeque::new();
    queue.push_back(initial_state);

    while let Some(state) = queue.pop_front() {
        for &spell in &[
            Spell::MagicMissile,
            Spell::Drain,
            Spell::Shield,
            Spell::Poison,
            Spell::Recharge,
        ] {
            let mut next_state = state.clone();
            if next_state.player_turn(spell) {
                next_state.boss_turn();
                if next_state.is_win() {
                    min_mana = min(min_mana, next_state.mana_spent);
                } else if !next_state.is_lose() && next_state.mana_spent < min_mana {
                    queue.push_back(next_state);
                }
            }
        }
    }
    min_mana
}

fn main() {
    let file = File::open("input.txt").expect("Failed to open input file");
    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    let boss_hp = lines
        .next()
        .unwrap()
        .unwrap()
        .split(": ")
        .nth(1)
        .unwrap()
        .parse()
        .unwrap();
    let boss_damage = lines
        .next()
        .unwrap()
        .unwrap()
        .split(": ")
        .nth(1)
        .unwrap()
        .parse()
        .unwrap();

    let part1_solution = solve(boss_hp, boss_damage, false);
    println!("Part 1: {}", part1_solution);

    let part2_solution = solve(boss_hp, boss_damage, true);
    println!("Part 2: {}", part2_solution);
}
