
import "io" for File

var content = File.read("input.txt")
var lines = content.split("\n")
var adj = {}
for (line in lines) {
  var trimmed = line.trim()
  if (trimmed == "") continue
  var words = trimmed.split(" ")
  if (words.count < 5 || words[4] == "no") continue
  var outer = words[0] + " " + words[1]
  var i = 5
  while (i < words.count) {
    var inner = words[i] + " " + words[i+1]
    if (!adj.containsKey(inner)) adj[inner] = []
    adj[inner].add(outer)
    i = i + 4
  }
}

var q = ["shiny gold"]
var visited = {}
var head = 0
while (head < q.count) {
  var curr = q[head]
  head = head + 1
  if (adj.containsKey(curr)) {
    for (parent in adj[curr]) {
      if (!visited.containsKey(parent)) {
        visited[parent] = true
        q.add(parent)
      }
    }
  }
}
System.print(visited.count)
