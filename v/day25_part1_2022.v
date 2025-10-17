
import os

fn from_snafu(s string) i64 {
    mut n := i64(0)
    for ch in s {
        n *= 5
        n += match ch {
            `=` { -2 }
            `-` { -1 }
            else { int(ch - `0`) }
        }
    }
    return n
}

fn to_snafu(n_ i64) string {
    mut n := n_
    mut b := []u8{}
    for n > 0 {
        match n % 5 {
            3 {
                n += 5
                b << `=`
            }
            4 {
                n += 5
                b << `-`
            }
            else {
                b << u8(`0` + n % 5)
            }
        }
        n /= 5
    }
    return b.reverse().bytestr()
}

fn main() {
    data := os.read_file('input.txt') or { panic(err) }
    mut sum := i64(0)
    for line in data.split_into_lines() {
        sum += from_snafu(line)
    }
    println(to_snafu(sum))
}
