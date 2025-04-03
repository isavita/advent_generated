
#!/usr/bin/awk -f

function nextVal(tag, criteria, factor, modulus) {
    do {
        vals[tag] = (vals[tag] * factor) % modulus
    } while (vals[tag] % criteria != 0)
    return vals[tag]
}

BEGIN {
    modulus = 2147483647
    factorA = 16807
    factorB = 48271
    criteriaA = 4
    criteriaB = 8
    limit = 5000000
    mask = 65536 # 2^16, for lower 16 bits comparison using modulo

    # Read starting values from input.txt
    getline line < "input.txt"
    split(line, partsA, " ")
    vals["A"] = partsA[length(partsA)]

    getline line < "input.txt"
    split(line, partsB, " ")
    vals["B"] = partsB[length(partsB)]

    close("input.txt")

    total = 0
    for (i = 0; i < limit; i++) {
        a = nextVal("A", criteriaA, factorA, modulus)
        b = nextVal("B", criteriaB, factorB, modulus)

        if ((a % mask) == (b % mask)) {
            total++
        }
    }

    print total
    exit # Important for BEGIN-only scripts
}
