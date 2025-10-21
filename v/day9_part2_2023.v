
import os

fn parse_input(input []string) [][]int {
    mut histories := [][]int{}
    for line in input {
        mut numbers := []int{}
        for num in line.split(' ') {
            numbers << num.int()
        }
        histories << numbers
    }
    return histories
}

fn all_zeros(nums []int) bool {
    for num in nums {
        if num != 0 {
            return false
        }
    }
    return true
}

fn calculate_extrapolation(history []int) []int {
    mut extrapolations := []int{}
    for i := 1; i < history.len; i++ {
        extrapolations << history[i] - history[i - 1]
    }
    return extrapolations
}

fn calculate_extrapolations(history []int) [][]int {
    mut series := [][]int{}
    series << history
    for i := 1; i < history.len; i++ {
        prev := series[i - 1]
        if all_zeros(prev) {
            return series
        }
        series << calculate_extrapolation(prev)
    }
    return series
}

fn solve(input []string) int {
    histories := parse_input(input)
    mut res := 0
    for history in histories {
        series := calculate_extrapolations(history)
        mut past := 0
        for i := series.len - 1; i >= 0; i-- {
            past = series[i][0] - past
        }
        res += past
    }
    return res
}

fn read_file(file_name string) []string {
    return os.read_lines(file_name) or { []string{} }
}

fn main() {
    input := read_file('input.txt')
    println(solve(input))
}
