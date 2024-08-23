import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    parts := input.split('\n\n')
    stack_lines := parts[0].split('\n')
    mut stacks := [][]string{len: stack_lines[stack_lines.len - 1].split(' ').filter(it != '').len}

    for line in stack_lines[..stack_lines.len - 1] {
        for i in 0 .. stacks.len {
            if line[1 + i * 4] != ` ` {
                stacks[i] << line[1 + i * 4].ascii_str()
            }
        }
    }

    for mut stack in stacks {
        stack.reverse_in_place()
    }

    moves := parts[1].split('\n')
    for move in moves {
        if move == '' { continue }
        move_parts := move.split(' ')
        count := move_parts[1].int()
        from := move_parts[3].int() - 1
        to := move_parts[5].int() - 1

        for _ in 0 .. count {
            crate := stacks[from].pop()
            stacks[to] << crate
        }
    }

    mut result := ''
    for stack in stacks {
        if stack.len > 0 {
            result += stack[stack.len - 1]
        }
    }
    println(result)
}