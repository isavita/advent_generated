
import os

const programs_len = 16

fn spin(mut programs []u8, x int) {
    mut t := programs.clone()
    for i in 0 .. programs_len {
        programs[(i + x) % programs_len] = t[i]
    }
}

fn exchange(mut programs []u8, a int, b int) {
    programs[a], programs[b] = programs[b], programs[a]
}

fn partner(mut programs []u8, a u8, b u8) {
    mut ai := -1
    mut bi := -1
    for i, c in programs {
        if c == a { ai = i }
        if c == b { bi = i }
    }
    if ai != -1 && bi != -1 {
        exchange(mut programs, ai, bi)
    }
}

fn main() {
    line := os.read_file('input.txt') or { panic(err) }.trim_space()
    mut programs := 'abcdefghijklmnop'.bytes()
    initial := programs.clone()
    mut cycle := 0

    for i := 0; i < 1_000_000_000; i++ {
        for move in line.split(',') {
            match move[0] {
                `s` {
                    x := move[1..].int()
                    spin(mut programs, x)
                }
                `x` {
                    parts := move[1..].split('/')
                    a := parts[0].int()
                    b := parts[1].int()
                    exchange(mut programs, a, b)
                }
                `p` {
                    partner(mut programs, move[1], move[3])
                }
                else {}
            }
        }
        if programs == initial {
            cycle = i + 1
            break
        }
    }

    programs = initial.clone()
    for _ in 0 .. 1_000_000_000 % cycle {
        for move in line.split(',') {
            match move[0] {
                `s` {
                    x := move[1..].int()
                    spin(mut programs, x)
                }
                `x` {
                    parts := move[1..].split('/')
                    a := parts[0].int()
                    b := parts[1].int()
                    exchange(mut programs, a, b)
                }
                `p` {
                    partner(mut programs, move[1], move[3])
                }
                else {}
            }
        }
    }
    println(programs.bytestr())
}
