
BEGIN {
    FS=","
}

{
    for (i=1; i<=NF; i++) {
        positions[NR, i] = $i
        max_pos = (max_pos < $i) ? $i : max_pos
    }
}

END {
    min_fuel = 1e9
    for (target=0; target<=max_pos; target++) {
        fuel = 0
        for (i=1; i<=NF; i++) {
            fuel += abs(positions[1, i] - target)
        }
        min_fuel = (fuel < min_fuel) ? fuel : min_fuel
    }
    print min_fuel
}

function abs(val) {
    return (val < 0) ? -val : val
}
