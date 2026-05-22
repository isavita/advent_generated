
import "io" for File

var content = File.read("input.txt").trim()
var fishes = List.filled(9, 0)
for (x in content.split(",")) {
  var f = Num.fromString(x.trim())
  fishes[f] = fishes[f] + 1
}

for (day in 1..80) {
  var newFish = fishes[0]
  for (i in 1..8) fishes[i - 1] = fishes[i]
  fishes[6] = fishes[6] + newFish
  fishes[8] = newFish
}

var total = 0
for (f in fishes) total = total + f
System.print(total)
