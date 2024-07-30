module main

import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    lines := input.split('\n')

    ip_index := lines[0].split(' ')[1].int()
    instructions := lines[1..].filter(it.len > 0)

    mut registers := [0, 0, 0, 0, 0, 0]
    mut ip := 0

    for ip < instructions.len {
        registers[ip_index] = ip
        parts := instructions[ip].split(' ')
        opcode := parts[0]
        a := parts[1].int()
        b := parts[2].int()
        c := parts[3].int()

        match opcode {
            'seti' { registers[c] = a }
            'setr' { registers[c] = registers[a] }
            'addi' { registers[c] = registers[a] + b }
            'addr' { registers[c] = registers[a] + registers[b] }
            'mulr' { registers[c] = registers[a] * registers[b] }
            'muli' { registers[c] = registers[a] * b }
            'banr' { registers[c] = registers[a] & registers[b] }
            'bani' { registers[c] = registers[a] & b }
            'bori' { registers[c] = registers[a] | b }
            'borr' { registers[c] = registers[a] | registers[b] }
            'gtir' { registers[c] = if a > registers[b] { 1 } else { 0 } }
            'gtri' { registers[c] = if registers[a] > b { 1 } else { 0 } }
            'gtrr' { registers[c] = if registers[a] > registers[b] { 1 } else { 0 } }
            'eqir' { registers[c] = if a == registers[b] { 1 } else { 0 } }
            'eqri' { registers[c] = if registers[a] == b { 1 } else { 0 } }
            'eqrr' { registers[c] = if registers[a] == registers[b] { 1 } else { 0 } }
            else { panic('Unknown opcode: $opcode') }
        }

        ip = registers[ip_index] + 1
    }

    println(registers[0])
}