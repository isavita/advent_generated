
use std::collections::HashSet;
use std::fs;

#[derive(Debug, Clone)]
struct Group {
    id: usize,
    units: i32,
    hit_points: i32,
    weaknesses: HashSet<String>,
    immunities: HashSet<String>,
    attack_damage: i32,
    attack_type: String,
    initiative: i32,
    army: String,
}

impl Group {
    fn effective_power(&self) -> i32 {
        self.units * self.attack_damage
    }

    fn damage_to(&self, other: &Group) -> i32 {
        if other.immunities.contains(&self.attack_type) {
            return 0;
        }
        let base = self.effective_power();
        if other.weaknesses.contains(&self.attack_type) {
            base * 2
        } else {
            base
        }
    }

    fn take_damage(&mut self, damage: i32) {
        let units_lost = damage / self.hit_points;
        self.units = (self.units - units_lost).max(0);
    }
}

fn parse_input(input: &str) -> Vec<Group> {
    let mut groups = Vec::new();
    let mut current_army = String::new();
    let mut id_counter = 0;

    for line in input.lines() {
        if line.is_empty() {
            continue;
        }
        if line == "Immune System:" {
            current_army = "Immune System".to_string();
            continue;
        } else if line == "Infection:" {
            current_army = "Infection".to_string();
            continue;
        }

        let parts: Vec<&str> = line.split_whitespace().collect();
        let units = parts[0].parse::<i32>().unwrap();
        let hit_points = parts[4].parse::<i32>().unwrap();

        let mut weaknesses = HashSet::new();
        let mut immunities = HashSet::new();

        if let Some(start) = line.find('(') {
            if let Some(end) = line.find(')') {
                let modifiers = &line[start + 1..end];
                for part in modifiers.split(';') {
                    let part = part.trim();
                    if part.starts_with("weak to") {
                        let types = part[7..].split(',');
                        for t in types {
                            weaknesses.insert(t.trim().to_string());
                        }
                    } else if part.starts_with("immune to") {
                        let types = part[9..].split(',');
                        for t in types {
                            immunities.insert(t.trim().to_string());
                        }
                    }
                }
            }
        }

        let attack_damage_index = line.find("does ").unwrap() + 5;
        let attack_damage_end = line[attack_damage_index..].find(' ').unwrap();
        let attack_damage = line[attack_damage_index..attack_damage_index + attack_damage_end]
            .parse::<i32>()
            .unwrap();

        let attack_type_index = attack_damage_index + attack_damage_end + 1;
        let attack_type_end = line[attack_type_index..].find(' ').unwrap();
        let attack_type = line[attack_type_index..attack_type_index + attack_type_end].to_string();

        let initiative_index = line.find("initiative ").unwrap() + 11;
        let initiative = line[initiative_index..].parse::<i32>().unwrap();

        groups.push(Group {
            id: id_counter,
            units,
            hit_points,
            weaknesses,
            immunities,
            attack_damage,
            attack_type,
            initiative,
            army: current_army.clone(),
        });
        id_counter += 1;
    }
    groups
}

fn simulate_combat(mut groups: Vec<Group>) -> i32 {
    loop {
        // Remove groups with no units
        groups.retain(|g| g.units > 0);

        let immune_system_units: i32 = groups
            .iter()
            .filter(|g| g.army == "Immune System")
            .map(|g| g.units)
            .sum();
        let infection_units: i32 = groups
            .iter()
            .filter(|g| g.army == "Infection")
            .map(|g| g.units)
            .sum();

        if immune_system_units == 0 || infection_units == 0 {
            return immune_system_units + infection_units;
        }

        // Target selection phase
        groups.sort_by_key(|g| (-g.effective_power(), -g.initiative));
        let mut targets = Vec::new();
        let mut targeted = HashSet::new();

        for group in groups.iter() {
            let enemy_groups: Vec<&Group> = groups
                .iter()
                .filter(|g| g.army != group.army && g.units > 0)
                .collect();

            let mut candidates: Vec<&Group> = enemy_groups
                .iter()
                .filter(|g| !targeted.contains(&g.id))
                .cloned()
                .collect();

            candidates.sort_by_key(|g| {
                (
                    -group.damage_to(g),
                    -g.effective_power(),
                    -g.initiative,
                )
            });

            if let Some(target) = candidates.first() {
                if group.damage_to(target) > 0 {
                    targeted.insert(target.id);
                    targets.push((group.id, target.id));
                }
            }
        }

        // Attacking phase
        targets.sort_by_key(|(attacker_id, _)| {
            let attacker = groups.iter().find(|g| g.id == *attacker_id).unwrap();
            -attacker.initiative
        });

        for (attacker_id, defender_id) in targets {
            let attacker = groups.iter().find(|g| g.id == attacker_id).cloned();
            let defender = groups.iter_mut().find(|g| g.id == defender_id);

            if let (Some(attacker), Some(defender)) = (attacker, defender) {
                if attacker.units <= 0 || defender.units <= 0 {
                    continue;
                }
                let damage = attacker.damage_to(defender);
                defender.take_damage(damage);
            }
        }
    }
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Failed to read input.txt");
    let groups = parse_input(&input);
    let result = simulate_combat(groups);
    println!("{}", result);
}
