
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')

root = {
  name: '/',
  type: 'dir',
  children: {},
  size: 0
}

currentDir = root
path = ['/']

for line in input
  if line.startsWith('$ cd')
    dirName = line.split(' ')[2]
    switch dirName
      when '/'
        currentDir = root
        path = ['/']
      when '..'
        path.pop()
        currentDir = root
        for dir in path[1..]
          currentDir = currentDir.children[dir]
      else
        path.push(dirName)
        currentDir = currentDir.children[dirName]
  else if line.startsWith('dir')
    dirName = line.split(' ')[1]
    currentDir.children[dirName] = {
      name: dirName,
      type: 'dir',
      children: {},
      size: 0
    }
  else if /^\d+/.test(line)
    [size, fileName] = line.split(' ')
    currentDir.children[fileName] = {
      name: fileName,
      type: 'file',
      size: parseInt(size)
    }

calculateSize = (dir) ->
  size = 0
  for childName, child of dir.children
    if child.type == 'dir'
      size += calculateSize(child)
    else
      size += child.size
  dir.size = size
  size

calculateSize(root)

sumOfSmallDirs = 0
findSmallDirs = (dir) ->
  if dir.type == 'dir' and dir.size <= 100000
    sumOfSmallDirs += dir.size
  for childName, child of dir.children
    if child.type == 'dir'
      findSmallDirs(child)

findSmallDirs(root)

console.log sumOfSmallDirs
