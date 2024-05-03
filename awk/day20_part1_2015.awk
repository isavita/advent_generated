#!/usr/bin/awk -f

BEGIN {
    target = 29000000  # Replace with your puzzle input
    house = 1
    presents = 0

    while (presents < target) {
        presents = 0
        for (elf = 1; elf * elf <= house; elf++) {
            if (house % elf == 0) {
                presents += 10 * elf
                if (elf * elf != house) {
                    presents += 10 * (house / elf)
                }
            }
        }
        house++
    }

    print house - 1
}