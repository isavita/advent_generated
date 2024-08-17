import "io" for File

var input = File.read("input.txt").trim()
var target = Num.fromString(input) / 10

var houses = List.filled(target + 1, 0)

for (elf in 1..target) {
  var house = elf
  while (house <= target) {
    houses[house] = houses[house] + elf
    house = house + elf
  }
}

for (houseNumber in 1..target) {
  if (houses[houseNumber] >= target) {
    System.print(houseNumber)
    break
  }
}