
import "io" for File

var input = File.read("input.txt")
var lines = input.split("\n")

var maxCalories = 0
var currentCalories = 0

for (line in lines) {
  if (line == "") {
    if (currentCalories > maxCalories) {
      maxCalories = currentCalories
    }
    currentCalories = 0
  } else {
    currentCalories = currentCalories + Num.fromString(line)
  }
}

// Check for the last elf
if (currentCalories > maxCalories) {
    maxCalories = currentCalories
}

System.print(maxCalories)
