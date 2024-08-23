import os

fn read_input(file string) []int {
    content := os.read_file(file) or { panic(err) }
    return content.split(',').map(it.int())
}

fn run_program(program []int, phase_setting int, input_signal int) int {
    mut memory := program.clone()
    mut input_count := 0
    mut output_signal := 0
    mut pc := 0

    for {
        opcode := memory[pc] % 100
        modes := [memory[pc] / 100 % 10, memory[pc] / 1000 % 10, memory[pc] / 10000 % 10]

        match opcode {
            1 {
                a := get_param(memory, pc + 1, modes[0])
                b := get_param(memory, pc + 2, modes[1])
                memory[memory[pc + 3]] = a + b
                pc += 4
            }
            2 {
                a := get_param(memory, pc + 1, modes[0])
                b := get_param(memory, pc + 2, modes[1])
                memory[memory[pc + 3]] = a * b
                pc += 4
            }
            3 {
                memory[memory[pc + 1]] = if input_count == 0 { phase_setting } else { input_signal }
                input_count++
                pc += 2
            }
            4 {
                output_signal = get_param(memory, pc + 1, modes[0])
                pc += 2
            }
            5 {
                a := get_param(memory, pc + 1, modes[0])
                b := get_param(memory, pc + 2, modes[1])
                pc = if a != 0 { b } else { pc + 3 }
            }
            6 {
                a := get_param(memory, pc + 1, modes[0])
                b := get_param(memory, pc + 2, modes[1])
                pc = if a == 0 { b } else { pc + 3 }
            }
            7 {
                a := get_param(memory, pc + 1, modes[0])
                b := get_param(memory, pc + 2, modes[1])
                memory[memory[pc + 3]] = if a < b { 1 } else { 0 }
                pc += 4
            }
            8 {
                a := get_param(memory, pc + 1, modes[0])
                b := get_param(memory, pc + 2, modes[1])
                memory[memory[pc + 3]] = if a == b { 1 } else { 0 }
                pc += 4
            }
            99 {
                return output_signal
            }
            else {
                panic('Unexpected opcode: $opcode')
            }
        }
    }
    return output_signal
}

fn get_param(memory []int, index int, mode int) int {
    return if mode == 0 { memory[memory[index]] } else { memory[index] }
}

fn permutations(arr []int) [][]int {
    mut result := [][]int{}
    mut arr_clone := arr.clone()
    permute(mut arr_clone, 0, mut result)
    return result
}

fn permute(mut arr []int, start int, mut result [][]int) {
    if start == arr.len {
        result << arr.clone()
        return
    }
    for i in start .. arr.len {
        arr[start], arr[i] = arr[i], arr[start]
        permute(mut arr, start + 1, mut result)
        arr[start], arr[i] = arr[i], arr[start]
    }
}

fn main() {
    program := read_input('input.txt')
    phase_settings := [0, 1, 2, 3, 4]
    mut max_signal := 0

    for perm in permutations(phase_settings) {
        mut signal := 0
        for phase in perm {
            signal = run_program(program, phase, signal)
        }
        if signal > max_signal {
            max_signal = signal
        }
    }

    println(max_signal)
}