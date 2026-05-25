
import "io" for File

var input = Num.fromString(File.read("input.txt").trim())

var limit = (input / 10).floor
var houses = List.filled(limit + 1, 0)

for (elf in 1..limit) {
    var house = elf
    while (house <= limit) {
        houses[house] = houses[house] + elf * 10
        house = house + elf
    }
}

var p1 = 0
for (i in 1..limit) {
    if (houses[i] >= input) {
        p1 = i
        break
    }
}
System.print("Part One: %(p1)")

var houses2 = List.filled(limit + 1, 0)
for (elf in 1..limit) {
    var house = elf
    var count = 0
    while (house <= limit && count < 50) {
        houses2[house] = houses2[house] + elf * 11
        house = house + elf
        count = count + 1
    }
}

var p2 = 0
for (i in 1..limit) {
    if (houses2[i] >= input) {
        p2 = i
        break
    }
}
System.print("Part Two: %(p2)")
