import os

fn main() {
    data := os.read_file('input.txt') or { panic(err) }
    lines := data.split('\n')
    player1_start := lines[0][28..].trim_space().int()
    player2_start := lines[1][28..].trim_space().int()

    mut player1_pos := player1_start
    mut player2_pos := player2_start
    mut player1_score := 0
    mut player2_score := 0
    mut die_roll := 1
    mut roll_count := 0

    for {
        mut rolls := (die_roll % 100) + ((die_roll + 1) % 100) + ((die_roll + 2) % 100)
        roll_count += 3
        die_roll += 3

        player1_pos = (player1_pos + rolls - 1) % 10 + 1
        player1_score += player1_pos
        if player1_score >= 1000 {
            println('Result: ${player2_score * roll_count}')
            break
        }

        rolls = (die_roll % 100) + ((die_roll + 1) % 100) + ((die_roll + 2) % 100)
        roll_count += 3
        die_roll += 3

        player2_pos = (player2_pos + rolls - 1) % 10 + 1
        player2_score += player2_pos
        if player2_score >= 1000 {
            println(player1_score * roll_count)
            break
        }
    }
}