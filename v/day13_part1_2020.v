import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    lines := input.split('\n')
    earliest_timestamp := lines[0].int()
    bus_ids := lines[1].split(',')

    mut min_wait_time := earliest_timestamp
    mut best_bus_id := 0

    for bus_id in bus_ids {
        if bus_id != 'x' {
            id := bus_id.int()
            wait_time := id - (earliest_timestamp % id)
            if wait_time < min_wait_time {
                min_wait_time = wait_time
                best_bus_id = id
            }
        }
    }

    println(best_bus_id * min_wait_time)
}