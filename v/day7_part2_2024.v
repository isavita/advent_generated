import os

fn concat_nums(n1 i64, n2 i64) i64 {
    s1 := n1.str()
    s2 := n2.str()
    combined := s1 + s2
    return combined.i64()
}

fn evaluate(target i64, numbers []i64) bool {
    n := numbers.len
    if n == 0 {
        return false
    }
    if n == 1 {
        return numbers[0] == target
    }
    num_ops := n - 1
    mut ops := []int{len: num_ops, init: 0}
    for {
        mut current := numbers[0]
        mut possible := true
        for i in 0 .. num_ops {
            next := numbers[i + 1]
            match ops[i] {
                0 { current += next }
                1 { current *= next }
                2 { current = concat_nums(current, next) }
                else {}
            }
        }
        if possible && current == target {
            return true
        }
        mut k := num_ops - 1
        for k >= 0 {
            ops[k]++
            if ops[k] < 3 {
                break
            }
            ops[k] = 0
            k--
        }
        if k < 0 {
            break
        }
    }
    return false
}

fn main() {
    data := os.read_file('input.txt') or { panic(err) }
    mut total := i64(0)
    for line in data.split_into_lines() {
        parts := line.split(':')
        target := parts[0].i64()
        nums := parts[1].split(' ').filter(it != '').map(it.i64())
        if evaluate(target, nums) {
            total += target
        }
    }
    println(total)
}