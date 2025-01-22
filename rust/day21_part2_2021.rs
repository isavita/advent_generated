
use std::fs;
use std::str::FromStr;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct GameState {
    p1_pos: usize,
    p2_pos: usize,
    p1_score: usize,
    p2_score: usize,
    turn: usize,
}

fn part1(p1_start: usize, p2_start: usize) -> usize {
    let mut p1_pos = p1_start;
    let mut p2_pos = p2_start;
    let mut p1_score = 0;
    let mut p2_score = 0;
    let mut die_rolls = 0;
    let mut die_val = 1;
    let mut turn = 0;

    loop {
        let mut move_amount = 0;
        for _ in 0..3 {
            move_amount += die_val;
            die_rolls += 1;
            die_val += 1;
            if die_val > 100 {
                die_val = 1;
            }
        }

        if turn % 2 == 0 {
            p1_pos = (p1_pos + move_amount - 1) % 10 + 1;
            p1_score += p1_pos;
            if p1_score >= 1000 {
                return p2_score * die_rolls;
            }
        } else {
            p2_pos = (p2_pos + move_amount - 1) % 10 + 1;
            p2_score += p2_pos;
            if p2_score >= 1000 {
                return p1_score * die_rolls;
            }
        }
        turn += 1;
    }
}

fn part2(p1_start: usize, p2_start: usize) -> usize {
    let mut memo: HashMap<GameState, (usize, usize)> = HashMap::new();
    let initial_state = GameState {
        p1_pos: p1_start,
        p2_pos: p2_start,
        p1_score: 0,
        p2_score: 0,
        turn: 0,
    };

    let (p1_wins, p2_wins) = count_wins(&initial_state, &mut memo);
    p1_wins.max(p2_wins)
}


fn count_wins(state: &GameState, memo: &mut HashMap<GameState, (usize, usize)>) -> (usize, usize) {
    if let Some(result) = memo.get(state) {
        return *result;
    }

     if state.p1_score >= 21 {
        return (1, 0);
    }
    if state.p2_score >= 21 {
        return (0, 1);
    }

    let mut p1_wins = 0;
    let mut p2_wins = 0;

    let possible_rolls = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)];
    
    for (roll_amount, freq) in possible_rolls {
        let next_state = if state.turn % 2 == 0 {
            let new_pos = (state.p1_pos + roll_amount - 1) % 10 + 1;
            GameState {
                p1_pos: new_pos,
                p2_pos: state.p2_pos,
                p1_score: state.p1_score + new_pos,
                p2_score: state.p2_score,
                turn: state.turn + 1,
            }
        } else {
             let new_pos = (state.p2_pos + roll_amount - 1) % 10 + 1;
             GameState {
                p1_pos: state.p1_pos,
                p2_pos: new_pos,
                p1_score: state.p1_score,
                p2_score: state.p2_score + new_pos,
                turn: state.turn + 1,
            }
        };
        let (wins1, wins2) = count_wins(&next_state, memo);
        p1_wins += wins1 * freq;
        p2_wins += wins2 * freq;
    }

    memo.insert(*state, (p1_wins, p2_wins));
    (p1_wins, p2_wins)
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Unable to read file");
    let lines: Vec<&str> = contents.lines().collect();
    
    let p1_start = usize::from_str(lines[0].split(": ").nth(1).unwrap()).unwrap();
    let p2_start = usize::from_str(lines[1].split(": ").nth(1).unwrap()).unwrap();
    

    println!("Part 1: {}", part1(p1_start, p2_start));
    println!("Part 2: {}", part2(p1_start, p2_start));
}
