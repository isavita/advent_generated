
use std::fs;
use std::str::FromStr;

#[derive(Debug, Default)]
struct Game {
    id: u32,
    rounds: Vec<Round>,
}

#[derive(Debug, Default)]
struct Round {
    red: u32,
    green: u32,
    blue: u32,
}

impl FromStr for Game {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split(':').collect();
        let game_id = parts[0]
            .split_whitespace()
            .nth(1)
            .unwrap()
            .parse::<u32>()
            .unwrap();
        let rounds_str = parts[1].trim();
        let rounds: Vec<Round> = rounds_str
            .split(';')
            .map(|round_str| {
                let mut round = Round::default();
                for cube_str in round_str.trim().split(',') {
                    let cube_parts: Vec<&str> = cube_str.trim().split_whitespace().collect();
                    let count = cube_parts[0].parse::<u32>().unwrap();
                    match cube_parts[1] {
                        "red" => round.red = count,
                        "green" => round.green = count,
                        "blue" => round.blue = count,
                        _ => (),
                    }
                }
                round
            })
            .collect();
        Ok(Game {
            id: game_id,
            rounds,
        })
    }
}

fn is_game_possible(game: &Game, red: u32, green: u32, blue: u32) -> bool {
    game.rounds
        .iter()
        .all(|round| round.red <= red && round.green <= green && round.blue <= blue)
}

fn calculate_min_power(game: &Game) -> u32 {
    let mut min_red = 0;
    let mut min_green = 0;
    let mut min_blue = 0;

    for round in &game.rounds {
        min_red = min_red.max(round.red);
        min_green = min_green.max(round.green);
        min_blue = min_blue.max(round.blue);
    }
    min_red * min_green * min_blue
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Unable to read file");
    let games: Vec<Game> = contents
        .lines()
        .filter_map(|line| Game::from_str(line).ok())
        .collect();

    let possible_games_sum: u32 = games
        .iter()
        .filter(|game| is_game_possible(game, 12, 13, 14))
        .map(|game| game.id)
        .sum();

    println!("Sum of IDs of possible games: {}", possible_games_sum);

    let total_power: u32 = games.iter().map(calculate_min_power).sum();
    println!("Total power: {}", total_power)
}
