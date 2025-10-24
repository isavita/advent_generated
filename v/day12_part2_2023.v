
import os

struct Row {
    springs []u8
    groups  []int
}

fn parse_line(line string) Row {
    parts := line.split(' ')
    springs := parts[0].bytes()
    
    mut groups := []int{}
    for g in parts[1].split(',') {
        groups << g.int()
    }
    
    return Row{springs, groups}
}

fn unfold(row Row) Row {
    mut new_springs := []u8{}
    mut new_groups := []int{}
    
    for i in 0..5 {
        if i > 0 {
            new_springs << u8(`?`)
        }
        new_springs << row.springs.clone()
        new_groups << row.groups.clone()
    }
    
    return Row{new_springs, new_groups}
}

struct Cache {
mut:
    data map[string]i64
}

fn (mut c Cache) get(key string) i64 {
    return c.data[key] or { -1 }
}

fn (mut c Cache) set(key string, value i64) {
    c.data[key] = value
}

fn count_arrangements(springs []u8, groups []int, mut cache Cache) i64 {
    key := '${springs.len},${groups.len},${springs[0] or { u8(0) }},${groups[0] or { 0 }}'
    
    cached := cache.get(key)
    if cached != -1 {
        return cached
    }
    
    if groups.len == 0 {
        result := if springs.contains(u8(`#`)) { 0 } else { 1 }
        cache.set(key, result)
        return result
    }
    
    if springs.len == 0 {
        cache.set(key, 0)
        return 0
    }
    
    mut result := i64(0)
    current := springs[0]
    
    if current == u8(`.`) || current == u8(`?`) {
        result += count_arrangements(springs[1..], groups, mut cache)
    }
    
    if current == u8(`#`) || current == u8(`?`) {
        group_size := groups[0]
        
        if springs.len >= group_size {
            mut can_place := true
            for i in 0..group_size {
                if springs[i] == u8(`.`) {
                    can_place = false
                    break
                }
            }
            
            if can_place && (springs.len == group_size || springs[group_size] != u8(`#`)) {
                new_springs := if springs.len > group_size {
                    springs[group_size + 1..]
                } else {
                    []u8{}
                }
                new_groups := groups[1..]
                result += count_arrangements(new_springs, new_groups, mut cache)
            }
        }
    }
    
    cache.set(key, result)
    return result
}

fn main() {
    content := os.read_file('input.txt') or { panic('Cannot read input.txt') }
    lines := content.trim_space().split('\n')
    
    mut part1 := i64(0)
    mut part2 := i64(0)
    
    for line in lines {
        row := parse_line(line)
        
        mut cache1 := Cache{map[string]i64{}}
        part1 += count_arrangements(row.springs, row.groups, mut cache1)
        
        unfolded := unfold(row)
        mut cache2 := Cache{map[string]i64{}}
        part2 += count_arrangements(unfolded.springs, unfolded.groups, mut cache2)
    }
    
    println('Part 1: $part1')
    println('Part 2: $part2')
}
