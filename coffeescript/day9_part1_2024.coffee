
fs = require 'fs'
line = fs.readFileSync('input.txt', 'utf-8').trim()
disk = []
fileID = 0
isFile = true
for char, i in line
  length = parseInt char
  if isFile
    for j in [0...length]
      disk.push fileID
    fileID++
  else
    for j in [0...length]
      disk.push -1
  isFile = not isFile
while true
  lfree = disk.indexOf -1
  break if lfree == -1
  rfile = -1
  for i in [disk.length - 1..lfree + 1] by -1
    if disk[i] != -1
      rfile = i
      break
  break if rfile == -1
  disk[lfree] = disk[rfile]
  disk[rfile] = -1
checksum = 0
for b, i in disk
  if b != -1
    checksum += i * b
console.log checksum
