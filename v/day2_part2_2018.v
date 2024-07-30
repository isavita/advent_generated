import os

fn main() {
    input_data := os.read_file('input.txt') or { panic(err) }
    box_ids := input_data.split_into_lines()

    mut two_count := 0
    mut three_count := 0

    for id in box_ids {
        mut char_count := map[string]int{}
        for c in id {
            char_count[c.ascii_str()]++
        }
        mut has_two := false
        mut has_three := false
        for count in char_count.values() {
            if count == 2 {
                has_two = true
            } else if count == 3 {
                has_three = true
            }
        }
        if has_two {
            two_count++
        }
        if has_three {
            three_count++
        }
    }
    checksum := two_count * three_count
    println('Checksum: $checksum')

    // Part Two
    for i in 0 .. box_ids.len {
        for j in i + 1 .. box_ids.len {
            diff_count := count_differences(box_ids[i], box_ids[j])
            if diff_count == 1 {
                common_letters := find_common_letters(box_ids[i], box_ids[j])
                println('Common letters: $common_letters')
                return
            }
        }
    }
}

fn count_differences(id1 string, id2 string) int {
    mut count := 0
    for i in 0 .. id1.len {
        if id1[i] != id2[i] {
            count++
        }
    }
    return count
}

fn find_common_letters(id1 string, id2 string) string {
    mut common := ''
    for i in 0 .. id1.len {
        if id1[i] == id2[i] {
            common += id1[i].ascii_str()
        }
    }
    return common
}