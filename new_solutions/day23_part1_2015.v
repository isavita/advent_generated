import os

fn main() {
    mut a := 0
    mut b := 0
    instructions := os.read_lines('input.txt') or { panic(err) }
    mut pc := 0

    for pc < instructions.len {
        parts := instructions[pc].split(' ')
        match parts[0] {
            'hlf' {
                if parts[1] == 'a' {
                    a /= 2
                } else {
                    b /= 2
                }
                pc++
            }
            'tpl' {
                if parts[1] == 'a' {
                    a *= 3
                } else {
                    b *= 3
                }
                pc++
            }
            'inc' {
                if parts[1] == 'a' {
                    a++
                } else {
                    b++
                }
                pc++
            }
            'jmp' {
                offset := parts[1].int()
                pc += offset
            }
            'jie' {
                offset := parts[2].int()
                if (parts[1][0] == `a` && a % 2 == 0) || (parts[1][0] == `b` && b % 2 == 0) {
                    pc += offset
                } else {
                    pc++
                }
            }
            'jio' {
                offset := parts[2].int()
                if (parts[1][0] == `a` && a == 1) || (parts[1][0] == `b` && b == 1) {
                    pc += offset
                } else {
                    pc++
                }
            }
            else {
                pc++
            }
        }
    }

    println(b)
}