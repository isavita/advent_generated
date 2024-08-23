import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    sections := input.split('\n\n')
    mut player1 := sections[0].split('\n')[1..].map(it.int())
    mut player2 := sections[1].split('\n')[1..].map(it.int())

    for player1.len > 0 && player2.len > 0 {
        card1 := player1[0]
        card2 := player2[0]
        player1 = player1[1..]
        player2 = player2[1..]
        if card1 > card2 {
            player1 << card1
            player1 << card2
        } else {
            player2 << card2
            player2 << card1
        }
    }

    winner := if player1.len > 0 { player1 } else { player2 }
    mut score := 0
    for i, card in winner {
        score += card * (winner.len - i)
    }
    println(score)
}