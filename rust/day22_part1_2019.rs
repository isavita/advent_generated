
fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let mut deck: Vec<usize> = (0..10007).collect();
    
    for line in input.trim().lines() {
        if line.starts_with("deal into new stack") {
            deck.reverse();
        } else if line.starts_with("cut") {
            let n: isize = line.split(" ").last().unwrap().parse().unwrap();
            if n > 0 {
                deck.rotate_left(n as usize);
            } else {
                deck.rotate_right(n.abs() as usize);
            }
        } else if line.starts_with("deal with increment") {
            let n: usize = line.split(" ").last().unwrap().parse().unwrap();
            let mut new_deck = vec![0; 10007];
            let mut pos = 0;
            for &card in &deck {
                new_deck[pos] = card;
                pos = (pos + n) % 10007;
            }
            deck = new_deck;
        }
    }
    
    let result = deck.iter().position(|&x| x == 2019).unwrap();
    println!("{}", result);
}
