
import os

fn spin(mut programs []u8, x int) {
    n := programs.len
    mut temp := []u8{len: n}
    for i in 0 .. n {
        temp[i] = programs[i]
    }
    for i in 0 .. n {
        programs[(i + x) % n] = temp[i]
    }
}

fn exchange(mut programs []u8, a int, b int) {
    programs[a], programs[b] = programs[b], programs[a]
}

fn partner(mut programs []u8, a u8, b u8) {
    mut ia := -1
    mut ib := -1
    for i, ch in programs {
        if ch == a {
            ia = i
        }
        if ch == b {
            ib = i
        }
    }
    exchange(mut programs, ia, ib)
}

fn main() {
    line := os.read_file('input.txt') or { panic(err) }
    mut programs := 'abcdefghijklmnop'.bytes()
    moves := line.trim_space().split(',')
    for move in moves {
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
                a := move[1]
                b := move[3]
                partner(mut programs, a, b)
            }
            else {}
        }
    }
    println(programs.bytestr())
}
