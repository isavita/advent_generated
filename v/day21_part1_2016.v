
import os

fn swap_position(mut password []u8, x int, y int) {
    password[x], password[y] = password[y], password[x]
}

fn swap_letter(mut password []u8, x u8, y u8) {
    for i, c in password {
        if c == x {
            password[i] = y
        } else if c == y {
            password[i] = x
        }
    }
}

fn rotate_left(mut password []u8, steps int) {
    n := steps % password.len
    mut temp := []u8{len: password.len}
    for i, c in password {
        temp[(i + password.len - n) % password.len] = c
    }
    for i, c in temp {
        password[i] = c
    }
}

fn rotate_right(mut password []u8, steps int) {
    n := steps % password.len
    mut temp := []u8{len: password.len}
    for i, c in password {
        temp[(i + n) % password.len] = c
    }
    for i, c in temp {
        password[i] = c
    }
}

fn rotate_based(mut password []u8, x u8) {
    mut idx := 0
    for i, c in password {
        if c == x {
            idx = i
            break
        }
    }
    steps := (1 + idx + if idx >= 4 { 1 } else { 0 }) % password.len
    rotate_right(mut password, steps)
}

fn reverse_positions(mut password []u8, x int, y int) {
    mut a := x
    mut b := y
    for a < b {
        swap_position(mut password, a, b)
        a++
        b--
    }
}

fn move_position(mut password []u8, x int, y int) {
    c := password[x]
    mut temp := []u8{}
    for i, ch in password {
        if i != x {
            temp << ch
        }
    }
    mut res := []u8{}
    for i, ch in temp {
        if i == y {
            res << c
        }
        res << ch
    }
    if y >= temp.len {
        res << c
    }
    password.clear()
    password << res
}

fn apply(line string, mut password []u8) {
    mut x := 0
    mut y := 0
    mut a := ''
    mut b := ''
    if line.starts_with('swap position') {
        x = line.split(' ')[2].int()
        y = line.split(' ')[5].int()
        swap_position(mut password, x, y)
    } else if line.starts_with('swap letter') {
        a = line.split(' ')[2]
        b = line.split(' ')[5]
        swap_letter(mut password, a[0], b[0])
    } else if line.starts_with('rotate left') {
        x = line.split(' ')[2].int()
        rotate_left(mut password, x)
    } else if line.starts_with('rotate right') {
        x = line.split(' ')[2].int()
        rotate_right(mut password, x)
    } else if line.starts_with('rotate based') {
        a = line.split(' ')[6]
        rotate_based(mut password, a[0])
    } else if line.starts_with('reverse') {
        parts := line.split(' ')
        x = parts[2].int()
        y = parts[4].int()
        reverse_positions(mut password, x, y)
    } else if line.starts_with('move') {
        parts := line.split(' ')
        x = parts[2].int()
        y = parts[5].int()
        move_position(mut password, x, y)
    }
}

fn main() {
    txt := os.read_file('input.txt') or { panic(err) }
    mut password := 'abcdefgh'.bytes()
    for line in txt.split_into_lines() {
        apply(line, mut password)
    }
    println(password.bytestr())
}
