import os

struct Reindeer {
    name       string
    speed      int
    fly_time   int
    rest_time  int
}

fn parse_reindeer(line string) Reindeer {
    parts := line.split(' ')
    return Reindeer{
        name: parts[0],
        speed: parts[3].int(),
        fly_time: parts[6].int(),
        rest_time: parts[13].int(),
    }
}

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    reindeer_list := input.split('\n').filter(it != '')

    mut max_distance := 0
    race_time := 2503

    for line in reindeer_list {
        r := parse_reindeer(line)
        cycle_time := r.fly_time + r.rest_time
        full_cycles := race_time / cycle_time
        remaining_time := race_time % cycle_time

        distance := (full_cycles * r.fly_time * r.speed) + 
                    (if remaining_time > r.fly_time { r.fly_time } else { remaining_time }) * r.speed
        
        if distance > max_distance {
            max_distance = distance
        }
    }

    println(max_distance)
}