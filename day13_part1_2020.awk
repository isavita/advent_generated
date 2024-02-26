
NR == 1 { earliest = $1 }
NR == 2 {
    split($0, buses, ",")
    for (i = 1; i <= length(buses); i++) {
        if (buses[i] != "x") {
            wait = buses[i] - (earliest % buses[i])
            if (wait < min_wait || min_wait == "") {
                min_wait = wait
                bus_id = buses[i]
            }
        }
    }
    print bus_id * min_wait
}
