
import os

fn get_combo(op int, a int, b int, c int) int {
    return if op <= 3 { op } else if op == 4 { a } else if op == 5 { b } else if op == 6 { c } else { -1 }
}

fn main() {
    txt := os.read_file('input.txt') or { panic(err) }
    mut a := 0
    mut b := 0
    mut c := 0
    mut prog := []int{}
    for line in txt.split_into_lines() {
        if line.len == 0 { continue }
        if line.starts_with('Register A:') {
            a = line[12..].int()
        } else if line.starts_with('Register B:') {
            b = line[12..].int()
        } else if line.starts_with('Register C:') {
            c = line[12..].int()
        } else if line.starts_with('Program:') {
            for n in line[9..].split(',') {
                prog << n.int()
            }
        }
    }
    mut out := []string{}
    mut ip := 0
    for ip < prog.len {
        op := prog[ip]
        if ip + 1 >= prog.len { break }
        operand := prog[ip + 1]
        match op {
            0 {
                d := get_combo(operand, a, b, c)
                a = if d == 0 { 0 } else { a >> d }
                ip += 2
            }
            1 {
                b ^= operand
                ip += 2
            }
            2 {
                b = get_combo(operand, a, b, c) & 7
                ip += 2
            }
            3 {
                ip = if a != 0 { operand } else { ip + 2 }
            }
            4 {
                b ^= c
                ip += 2
            }
            5 {
                out << (get_combo(operand, a, b, c) & 7).str()
                ip += 2
            }
            6 {
                d := get_combo(operand, a, b, c)
                b = a >> d
                ip += 2
            }
            7 {
                d := get_combo(operand, a, b, c)
                c = a >> d
                ip += 2
            }
            else {}
        }
    }
    println(out.join(','))
}
