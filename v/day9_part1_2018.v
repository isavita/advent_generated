import os

fn main() {
    input := os.read_file('input.txt') or { return }
    parts := input.split(' ')
    players := parts[0].int()
    last_marble := parts[6].int()

    mut scores := []int{len: players, init: 0}
    mut marbles := [0]
    mut current_index := 0

    for marble in 1 .. last_marble + 1 {
        if marble % 23 == 0 {
            current_index = (current_index - 7 + marbles.len) % marbles.len
            scores[marble % players] += marble + marbles[current_index]
            marbles.delete(current_index)
        } else {
            current_index = (current_index + 2) % marbles.len
            marbles.insert(current_index, marble)
        }
    }

    mut max_score := 0
    for score in scores {
        if score > max_score {
            max_score = score
        }
    }

    println(max_score)
}