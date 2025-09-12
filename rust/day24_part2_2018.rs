
use std::collections::{HashMap, HashSet};
use std::fs;

#[derive(Clone)]
struct Group {
    units: i32,
    hit_points: i32,
    attack_damage: i32,
    attack_type: String,
    initiative: i32,
    immunities: HashSet<String>,
    weaknesses: HashSet<String>,
    target: Option<usize>,
    attacker: Option<usize>,
}

impl Group {
    fn effective_power(&self) -> i64 {
        self.units as i64 * self.attack_damage as i64
    }

    fn damage_dealt(&self, enemy: &Group) -> i64 {
        if enemy.immunities.contains(&self.attack_type) {
            return 0;
        }
        if enemy.weaknesses.contains(&self.attack_type) {
            return self.effective_power() * 2;
        }
        self.effective_power()
    }
}

struct Battlefield {
    groups: Vec<Group>,
    group_to_army: Vec<i32>,
}

impl Battlefield {
    fn clone(&self) -> Self {
        Battlefield {
            groups: self.groups.clone(),
            group_to_army: self.group_to_army.clone(),
        }
    }

    fn boost(&mut self, amount: i32) {
        for (i, group) in self.groups.iter_mut().enumerate() {
            if self.group_to_army[i] == 1 {
                group.attack_damage += amount;
            }
        }
    }

    fn find_targets(&mut self) {
        for group in &mut self.groups {
            group.target = None;
            group.attacker = None;
        }

        let mut indices: Vec<usize> = (0..self.groups.len()).collect();
        indices.sort_by(|&a, &b| {
            let ep_a = self.groups[a].effective_power();
            let ep_b = self.groups[b].effective_power();
            ep_b.cmp(&ep_a).then_with(|| self.groups[b].initiative.cmp(&self.groups[a].initiative))
        });

        for &i in &indices {
            if self.groups[i].units <= 0 {
                continue;
            }

            let mut best_damage = 0;
            let mut best_target: Option<usize> = None;

            for &j in &indices {
                if self.groups[j].units <= 0
                    || self.group_to_army[i] == self.group_to_army[j]
                    || self.groups[j].attacker.is_some()
                {
                    continue;
                }

                let damage = self.groups[i].damage_dealt(&self.groups[j]);
                if damage == 0 {
                    continue;
                }

                if best_target.is_none() || damage > best_damage {
                    best_damage = damage;
                    best_target = Some(j);
                } else if damage == best_damage {
                    let current_ep = self.groups[j].effective_power();
                    let best_ep = self.groups[best_target.unwrap()].effective_power();
                    if current_ep > best_ep {
                        best_target = Some(j);
                    } else if current_ep == best_ep {
                        if self.groups[j].initiative > self.groups[best_target.unwrap()].initiative {
                            best_target = Some(j);
                        }
                    }
                }
            }

            if let Some(target_idx) = best_target {
                self.groups[i].target = Some(target_idx);
                self.groups[target_idx].attacker = Some(i);
            }
        }
    }

    fn attack(&mut self) {
        let mut indices: Vec<usize> = (0..self.groups.len()).collect();
        indices.sort_by(|&a, &b| self.groups[b].initiative.cmp(&self.groups[a].initiative));

        for &i in &indices {
            if self.groups[i].units <= 0 {
                continue;
            }
            if let Some(target_idx) = self.groups[i].target {
                if self.groups[target_idx].units <= 0 {
                    continue;
                }
                let damage = self.groups[i].damage_dealt(&self.groups[target_idx]);
                let units_lost = damage / self.groups[target_idx].hit_points as i64;
                self.groups[target_idx].units -= units_lost as i32;
            }
        }
    }

    fn clean(&mut self) {
        let mut to_remove = Vec::new();
        for (i, group) in self.groups.iter().enumerate() {
            if group.units <= 0 {
                to_remove.push(i);
            }
        }
        for &i in to_remove.iter().rev() {
            self.groups.remove(i);
            self.group_to_army.remove(i);
        }
    }

    fn active(&self) -> bool {
        let mut army1_alive = false;
        let mut army2_alive = false;
        for (i, group) in self.groups.iter().enumerate() {
            if group.units > 0 {
                if self.group_to_army[i] == 1 {
                    army1_alive = true;
                } else {
                    army2_alive = true;
                }
            }
        }
        army1_alive && army2_alive
    }

    fn result(&self) -> (i32, i32) {
        let mut winner_id = 0;
        let mut units_left = 0;
        for (i, group) in self.groups.iter().enumerate() {
            if group.units > 0 {
                if winner_id == 0 {
                    winner_id = self.group_to_army[i];
                }
                units_left += group.units;
            }
        }
        (winner_id, units_left)
    }

    fn total_units(&self) -> i64 {
        self.groups.iter().map(|g| g.units as i64).sum()
    }
}

fn parse_input(input_data: &str) -> Battlefield {
    let mut groups = Vec::new();
    let mut group_to_army = Vec::new();
    let mut current_army_id = 0;

    for line in input_data.lines() {
        if line.is_empty() {
            continue;
        }
        if line.ends_with(':') {
            if line.starts_with("Immune System") {
                current_army_id = 1;
            } else if line.starts_with("Infection") {
                current_army_id = 2;
            }
            continue;
        }

        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() < 10 {
            continue;
        }

        let units = parts[0].parse().unwrap();
        let hit_points = parts[4].parse().unwrap();
        let attack_damage = parts[parts.len() - 6].parse().unwrap();
        let attack_type = parts[parts.len() - 5].to_string();
        let initiative = parts[parts.len() - 1].parse().unwrap();

        let mut immunities = HashSet::new();
        let mut weaknesses = HashSet::new();

        if let Some(start) = line.find('(') {
            if let Some(end) = line.find(')') {
                let modifiers = &line[start + 1..end];
                for part in modifiers.split(';') {
                    let part = part.trim();
                    if part.starts_with("immune to") {
                        let types = part[10..].split(',');
                        for t in types {
                            immunities.insert(t.trim().to_string());
                        }
                    } else if part.starts_with("weak to") {
                        let types = part[7..].split(',');
                        for t in types {
                            weaknesses.insert(t.trim().to_string());
                        }
                    }
                }
            }
        }

        groups.push(Group {
            units,
            hit_points,
            attack_damage,
            attack_type,
            initiative,
            immunities,
            weaknesses,
            target: None,
            attacker: None,
        });
        group_to_army.push(current_army_id);
    }

    Battlefield {
        groups,
        group_to_army,
    }
}

fn simulate_battle(initial: &Battlefield, boost_amount: i32) -> (i32, i32) {
    let mut battle = initial.clone();
    battle.boost(boost_amount);

    let mut prev_total_units = -1;

    while battle.active() {
        let before_units = battle.total_units();
        battle.find_targets();
        battle.attack();
        battle.clean();

        let after_units = battle.total_units();
        if after_units == before_units {
            return (0, 0);
        }
        prev_total_units = after_units;
    }

    battle.result()
}

fn immune_system_boost(input_data: &str) -> i32 {
    let initial = parse_input(input_data);

    let mut low = 0;
    let mut high = 200000;
    let mut winning_units = 0;

    while low <= high {
        let mid = low + (high - low) / 2;
        let (winner, units) = simulate_battle(&initial, mid);

        if winner == 1 {
            winning_units = units;
            high = mid - 1;
        } else {
            low = mid + 1;
        }
    }

    winning_units
}

fn main() {
    let input_data = fs::read_to_string("input.txt").unwrap();
    println!("{}", immune_system_boost(&input_data));
}
