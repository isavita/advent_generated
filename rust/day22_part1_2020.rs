
fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let mut players = input.split("\n\n");

    let player1: Vec<usize> = players.next().unwrap().lines().skip(1).map(|x| x.parse().unwrap()).collect();
    let player2: Vec<usize> = players.next().unwrap().lines().skip(1).map(|x| x.parse().unwrap()).collect();

    let winner = play_game(player1, player2);

    let score: usize = winner.iter().rev().enumerate().map(|(i, &x)| x * (i + 1)).sum();
    println!("{}", score);
}

fn play_game(mut player1: Vec<usize>, mut player2: Vec<usize>) -> Vec<usize> {
    while !player1.is_empty() && !player2.is_empty() {
        let card1 = player1.remove(0);
        let card2 = player2.remove(0);

        if card1 > card2 {
            player1.push(card1);
            player1.push(card2);
        } else {
            player2.push(card2);
            player2.push(card1);
        }
    }

    if player1.is_empty() {
        player2
    } else {
        player1
    }
}
