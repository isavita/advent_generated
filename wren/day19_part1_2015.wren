
import "io" for File

var replacements = []
var molecule = ""

var file = File.open("input.txt")
var lines = file.readBytes(file.size).toString.split("\n")
for (line in lines) {
  if (line == "") continue
  if (line.contains(" => ")) {
    replacements.add(line)
  } else {
    molecule = line
  }
}
file.close()

var molecules = {}
for (replacement in replacements) {
  var parts = replacement.split(" => ")
  var from = parts[0]
  var to = parts[1]
  var indices = []
  var start = 0
  while (true) {
    var index = molecule.indexOf(from, start)
    if (index == -1) break
    indices.add(index)
    start = index + 1
  }
  for (index in indices) {
    var newMolecule = molecule[0...index] + to + molecule[index+from.count..-1]
    molecules[newMolecule] = true
  }
}

System.print(molecules.count)
