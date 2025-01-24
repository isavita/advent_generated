
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct State {
    player_hp: i32,
    player_mana: i32,
    boss_hp: i32,
    boss_damage: i32,
    shield_timer: i32,
    poison_timer: i32,
    recharge_timer: i32,
    mana_spent: i32,
}

fn spell_cost(spell: Spell) -> i32 {
    match spell {
        Spell::MagicMissile => 53,
        Spell::Drain => 73,
        Spell::Shield => 113,
        Spell::Poison => 173,
        Spell::Recharge => 229,
    }
}

fn apply_effects(state: &mut State) {
    if state.shield_timer > 0 {
        state.shield_timer -= 1;
    }
    if state.poison_timer > 0 {
        state.boss_hp -= 3;
        state.poison_timer -= 1;
    }
    if state.recharge_timer > 0 {
        state.player_mana += 101;
        state.recharge_timer -= 1;
    }
}

fn player_turn(state: &State, spell: Spell) -> Option<State> {
    let mut new_state = state.clone();
    apply_effects(&mut new_state);

    if new_state.boss_hp <= 0 {
        return Some(new_state);
    }

    if new_state.player_mana < spell_cost(spell) {
        return None;
    }

    new_state.player_mana -= spell_cost(spell);
    new_state.mana_spent += spell_cost(spell);

    match spell {
        Spell::MagicMissile => new_state.boss_hp -= 4,
        Spell::Drain => {
            new_state.boss_hp -= 2;
            new_state.player_hp += 2;
        }
        Spell::Shield => {
            if new_state.shield_timer > 0 {
                return None;
            }
            new_state.shield_timer = 6;
        }
        Spell::Poison => {
            if new_state.poison_timer > 0 {
                return None;
            }
            new_state.poison_timer = 6;
        }
        Spell::Recharge => {
            if new_state.recharge_timer > 0 {
                return None;
            }
            new_state.recharge_timer = 5;
        }
    }

    Some(new_state)
}

fn boss_turn(state: &State) -> State {
    let mut new_state = state.clone();
    apply_effects(&mut new_state);

    if new_state.boss_hp <= 0 {
        return new_state;
    }

    let armor = if new_state.shield_timer > 0 { 7 } else { 0 };
    let damage = i32::max(1, new_state.boss_damage - armor);
    new_state.player_hp -= damage;
    new_state
}

fn solve() -> i32 {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    let boss_hp = lines
        .next()
        .unwrap()
        .unwrap()
        .split(": ")
        .nth(1)
        .unwrap()
        .parse::<i32>()
        .unwrap();
    let boss_damage = lines
        .next()
        .unwrap()
        .unwrap()
        .split(": ")
        .nth(1)
        .unwrap()
        .parse::<i32>()
        .unwrap();

    let initial_state = State {
        player_hp: 50,
        player_mana: 500,
        boss_hp,
        boss_damage,
        shield_timer: 0,
        poison_timer: 0,
        recharge_timer: 0,
        mana_spent: 0,
    };

    let mut min_mana = i32::MAX;
    let mut q = VecDeque::new();
    q.push_back(initial_state);

    while let Some(state) = q.pop_front() {
        for &spell in &[
            Spell::MagicMissile,
            Spell::Drain,
            Spell::Shield,
            Spell::Poison,
            Spell::Recharge,
        ] {
            if let Some(next_state) = player_turn(&state, spell) {
                let final_state = boss_turn(&next_state);
                if final_state.player_hp <= 0 {
                    continue;
                }
                if final_state.boss_hp <= 0 {
                    min_mana = min(min_mana, final_state.mana_spent);
                } else if final_state.mana_spent < min_mana {
                    q.push_back(final_state);
                }
            }
        }
    }
    min_mana
}

fn main() {
    let result = solve();
    println!("{}", result);
}
