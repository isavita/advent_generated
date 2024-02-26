
BEGIN {
    FS = ")"
}

{
    orbits[$2] = $1
    obj = $2
    while (orbits[obj] in orbits) {
        obj = orbits[obj]
        count++
    }
}

END {
    sum = 0
    for (obj in orbits) {
        while (obj in orbits) {
            sum++
            obj = orbits[obj]
        }
    }
    print sum
}
