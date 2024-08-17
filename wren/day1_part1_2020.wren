import "io" for File

var input = File.read("input.txt")
var lines = input.split("\n")
var numbers = []

for (line in lines) {
  if (line.trim() != "") {
    numbers.add(Num.fromString(line))
  }
}

for (i in 0...numbers.count - 1) {
  for (j in i + 1...numbers.count) {
    if (numbers[i] + numbers[j] == 2020) {
      System.print(numbers[i] * numbers[j])
      return
    }
  }
}