BEGIN {
    # Read the input number from input.txt
    getline input < "input.txt"
    input = int(input)

    # Initialize variables
    house = 1
    presents = 0

    # Part One
    while (presents < input) {
        presents = 0
        for (elf = 1; elf * elf <= house; elf++) {
            if (house % elf == 0) {
                presents += elf * 10
                if (elf != house / elf) {
                    presents += (house / elf) * 10
                }
            }
        }
        house++
    }
    print "Part One: " house - 1

    # Reset variables for Part Two
    house = 1
    presents = 0
    max_houses = 50

    # Part Two
    while (presents < input) {
        presents = 0
        for (elf = 1; elf * elf <= house; elf++) {
            if (house % elf == 0) {
                if (house <= elf * max_houses) {
                    presents += elf * 11
                }
                if (elf != house / elf && house <= (house / elf) * max_houses) {
                    presents += (house / elf) * 11
                }
            }
        }
        house++
    }
    print "Part Two: " house - 1
}