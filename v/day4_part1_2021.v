module main

import os
import strconv

struct Board {
mut:
	numbers [][]int
	marked  [][]bool
}

fn main() {
	input := os.read_file('input.txt') or { panic(err) }
	sections := input.split('\n\n')
	draws := sections[0].split(',').map(strconv.atoi(it) or { 0 })
	mut boards := []Board{}

	for i in 1 .. sections.len {
		mut board := Board{
			numbers: [][]int{len: 5, init: []int{len: 5}},
			marked:  [][]bool{len: 5, init: []bool{len: 5}},
		}
		rows := sections[i].split('\n')
		for j in 0 .. rows.len {
			nums := rows[j].split(' ').filter(it != '')
			for k in 0 .. nums.len {
				board.numbers[j][k] = strconv.atoi(nums[k]) or { 0 }
			}
		}
		boards << board
	}

	for draw in draws {
		for mut board in boards {
			mark_number(mut board, draw)
			if is_winner(board) {
				score := calculate_score(board, draw)
				println(score)
				return
			}
		}
	}
}

fn mark_number(mut board Board, number int) {
	for i in 0 .. board.numbers.len {
		for j in 0 .. board.numbers[i].len {
			if board.numbers[i][j] == number {
				board.marked[i][j] = true
			}
		}
	}
}

fn is_winner(board Board) bool {
	for i in 0 .. board.marked.len {
		if board.marked[i].all(it) || board.marked.map(it[i]).all(it) {
			return true
		}
	}
	return false
}

fn calculate_score(board Board, last_draw int) int {
	mut sum := 0
	for i in 0 .. board.numbers.len {
		for j in 0 .. board.numbers[i].len {
			if !board.marked[i][j] {
				sum += board.numbers[i][j]
			}
		}
	}
	return sum * last_draw
}