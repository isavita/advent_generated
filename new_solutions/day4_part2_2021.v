import os

struct Board {
mut:
    numbers [][]int
    marks   [][]bool
}

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    lines := input.split('\n')

    draws := lines[0].split(',').map(it.int())
    mut boards := []Board{}

    for i := 2; i < lines.len; i += 6 {
        mut board := Board{
            numbers: [][]int{len: 5, init: []int{len: 5}},
            marks:   [][]bool{len: 5, init: []bool{len: 5}},
        }
        for j := 0; j < 5; j++ {
            board.numbers[j] = lines[i + j].split(' ').filter(it != '').map(it.int())
        }
        boards << board
    }

    mut last_score := 0
    mut won_boards := []int{}

    for draw in draws {
        for b in 0 .. boards.len {
            if b in won_boards {
                continue
            }
            mark_board(mut boards[b], draw)
            if check_winner(boards[b]) {
                won_boards << b
                last_score = calculate_score(boards[b], draw)
            }
        }
    }

    println(last_score)
}

fn mark_board(mut board Board, draw int) {
    for i in 0 .. board.numbers.len {
        for j in 0 .. board.numbers[i].len {
            if board.numbers[i][j] == draw {
                board.marks[i][j] = true
            }
        }
    }
}

fn check_winner(board Board) bool {
    for i in 0 .. board.marks.len {
        if board.marks[i].all(it) || board.marks.map(it[i]).all(it) {
            return true
        }
    }
    return false
}

fn calculate_score(board Board, draw int) int {
    mut sum := 0
    for i in 0 .. board.numbers.len {
        for j in 0 .. board.numbers[i].len {
            if !board.marks[i][j] {
                sum += board.numbers[i][j]
            }
        }
    }
    return sum * draw
}