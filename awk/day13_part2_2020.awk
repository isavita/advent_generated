#!/usr/bin/awk

BEGIN {
    getline timestamp < "input.txt"
    getline buses < "input.txt"
    split(buses, bus_ids, ",")
    min_wait = 1000000
    min_bus = 0
    for (i in bus_ids) {
        if (bus_ids[i] != "x") {
            bus_id = bus_ids[i] + 0
            wait = bus_id - (timestamp % bus_id)
            if (wait < min_wait) {
                min_wait = wait
                min_bus = bus_id
            }
        }
    }
    print min_bus * min_wait

    # Part 2
    timestamp = 0
    step = 1
    for (i in bus_ids) {
        if (bus_ids[i] != "x") {
            bus_id = bus_ids[i] + 0
            while ((timestamp + i - 1) % bus_id != 0) {
                timestamp += step
            }
            step *= bus_id
        }
    }
    print timestamp
}