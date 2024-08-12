BEGIN {
    while ((getline < "input.txt") > 0) {
        polymer = $0
    }
    while (1) {
        new_polymer = ""
        i = 1
        while (i <= length(polymer)) {
            if (i < length(polymer) && tolower(substr(polymer, i, 1)) == tolower(substr(polymer, i + 1, 1)) && substr(polymer, i, 1) != substr(polymer, i + 1, 1)) {
                i += 2
            } else {
                new_polymer = new_polymer substr(polymer, i, 1)
                i++
            }
        }
        if (new_polymer == polymer) break
        polymer = new_polymer
    }
    print length(polymer)
}