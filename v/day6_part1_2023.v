
import os

fn ways(time i64, record i64) i64 {
    mut low := i64(1)
    mut high := time - 1
    for low <= high {
        mid := low + (high - low) / 2
        if mid * (time - mid) > record {
            high = mid - 1
        } else {
            low = mid + 1
        }
    }
    return time - 2 * low + 1
}

fn main() {
    data := os.read_file('input.txt') or { panic(err) }
    mut times := []i64{}
    mut dists := []i64{}
    for line in data.split_into_lines() {
        if line.starts_with('Time') {
            for tok in line[10..].split(' ') {
                if tok != '' { times << tok.i64() }
            }
        } else if line.starts_with('Distance') {
            for tok in line[10..].split(' ') {
                if tok != '' { dists << tok.i64() }
            }
        }
    }
    mut prod := i64(1)
    for i in 0 .. times.len {
        prod *= ways(times[i], dists[i])
    }
    println(prod)
}
