import os

fn main() {
    mut happiness := map[string]map[string]int{}
    mut guests := []string{}

    lines := os.read_lines('input.txt') or { return }
    for line in lines {
        parts := line.split(' ')
        person1 := parts[0]
        person2 := parts[10].trim_right('.')
        mut units := parts[3].int()
        if parts[2] == 'lose' {
            units = -units
        }

        if person1 !in happiness {
            happiness[person1] = map[string]int{}
            guests << person1
        }
        happiness[person1][person2] = units
    }

    max_happiness := find_max_happiness(happiness, mut guests, 0)
    println(max_happiness)
}

fn find_max_happiness(happiness map[string]map[string]int, mut guests []string, start int) int {
    if start == guests.len - 1 {
        return calculate_happiness(happiness, guests)
    }

    mut max_happiness := int(0)
    for i in start .. guests.len {
        guests[start], guests[i] = guests[i], guests[start]
        current_happiness := find_max_happiness(happiness, mut guests, start + 1)
        if current_happiness > max_happiness {
            max_happiness = current_happiness
        }
        guests[start], guests[i] = guests[i], guests[start]
    }
    return max_happiness
}

fn calculate_happiness(happiness map[string]map[string]int, guests []string) int {
    mut total := 0
    for i in 0 .. guests.len {
        left := guests[(i - 1 + guests.len) % guests.len]
        right := guests[(i + 1) % guests.len]
        total += happiness[guests[i]][left] + happiness[guests[i]][right]
    }
    return total
}