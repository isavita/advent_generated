
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')

root = [""]
dirs = {}
files = {}
curr = []

for line in input
  txt = line.split(/\s+/)
  if txt[0] is "$"
    if txt[1] is "cd"
      if txt[2] is "/"
        curr = root
      else if txt[2] is ".."
        curr.pop()
      else
        curr.push txt[2]
      dirs[curr.join("/")] = 0
  else
    if txt[0] isnt "dir"
      files[curr.concat(txt[1]).join("/")] = parseInt(txt[0])

for file, size of files
  path = file.split("/")
  for i in [1...path.length]
    dirs[path.slice(0, i).join("/")] += size

sortedSizes = Object.values(dirs).sort (a, b) -> a - b
total = 70000000
want = 30000000
available = total - dirs[""]
index = sortedSizes.findIndex (size) -> size >= want - available
console.log sortedSizes[index]
