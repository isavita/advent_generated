
import os

fn extended_gcd(a i64, b i64, mut x &i64, mut y &i64) i64 {
    if a == 0 {
        unsafe {
            *x = 0
            *y = 1
        }
        return b
    }
    mut x1, mut y1 := i64(0), i64(0)
    gcd := extended_gcd(b % a, a, mut &x1, mut &y1)
    unsafe {
        *x = y1 - (b / a) * x1
        *y = x1
    }
    return gcd
}

fn find_earliest_timestamp(ids []i64, offsets []i64) i64 {
    mut n := i64(1)
    for id in ids {
        n *= id
    }
    mut result := i64(0)
    for i, id in ids {
        ni := n / id
        mut xi, mut yi := i64(0), i64(0)
        extended_gcd(ni, id, mut &xi, mut &yi)
        result += ((-offsets[i] % id + id) % id * xi % n * ni % n + n) % n
    }
    return result % n
}

fn main() {
    content := os.read_file('input.txt') or { panic(err) }
    lines := content.trim('\n').split('\n')
    mut ids := []i64{}
    mut offsets := []i64{}
    mut offset := 0
    for tok in lines[1].split(',') {
        if tok != 'x' {
            ids << tok.i64()
            offsets << offset
        }
        offset++
    }
    println(find_earliest_timestamp(ids, offsets))
}
