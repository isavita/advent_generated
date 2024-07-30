import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    rounds := input.split_into_lines()
    
    mut total_score := 0
    for round in rounds {
        parts := round.split(' ')
        opponent := parts[0]
        outcome := parts[1]

        player_choice := calculate_choice(opponent, outcome)
        total_score += calculate_score(player_choice, opponent, outcome)
    }
    
    println(total_score)
}

fn calculate_choice(opponent string, outcome string) string {
    match outcome {
        'X' { return match opponent {
            'A' { 'C' } // Lose against Rock
            'B' { 'A' } // Lose against Paper
            'C' { 'B' } // Lose against Scissors
            else { '' }
        }}
        'Y' { return opponent } // Draw
        'Z' { return match opponent {
            'A' { 'B' } // Win against Rock
            'B' { 'C' } // Win against Paper
            'C' { 'A' } // Win against Scissors
            else { '' }
        }}
        else { return '' }
    }
}

fn calculate_score(player_choice string, opponent string, outcome string) int {
    shape_score := match player_choice {
        'A' { 1 } // Rock
        'B' { 2 } // Paper
        'C' { 3 } // Scissors
        else { 0 }
    }
    
    outcome_score := match outcome {
        'X' { 0 } // Loss
        'Y' { 3 } // Draw
        'Z' { 6 } // Win
        else { 0 }
    }
    
    return shape_score + outcome_score
}