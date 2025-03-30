
import std/[strutils, tables, sequtils, os]

type
  DirectoryRef = ref Directory
  Directory = object
    files: Table[string, int]
    directories: Table[string, DirectoryRef]

var totalSumLessThan100k: int = 0

proc calculateSizeAndSum(d: DirectoryRef): int =
  result = 0
  for size in d.files.values:
    result += size
  for subdir in d.directories.values:
    result += calculateSizeAndSum(subdir)

  if result <= 100000:
    totalSumLessThan100k += result
  
  return result

proc main() =
  totalSumLessThan100k = 0 
  var root = DirectoryRef(files: initTable[string, int](), directories: initTable[string, DirectoryRef]())
  var currentDir = root
  var directoryStack: seq[DirectoryRef] = @[root]

  let f = open("input.txt")
  defer: f.close()

  for line in f.lines:
    let parts = line.strip.split(' ')
    if parts[0] == "$" and parts[1] == "cd":
      let path = parts[2]
      if path == "/":
        currentDir = root
        directoryStack = @[root]
      elif path == "..":
        if directoryStack.len > 1:
          discard directoryStack.pop()
          currentDir = directoryStack[^1]
      else:
        if not currentDir.directories.contains(path):
          currentDir.directories[path] = DirectoryRef(files: initTable[string, int](), directories: initTable[string, DirectoryRef]())
        currentDir = currentDir.directories[path]
        directoryStack.add(currentDir)
    elif parts[0] == "dir":
      let dirName = parts[1]
      if not currentDir.directories.contains(dirName):
        currentDir.directories[dirName] = DirectoryRef(files: initTable[string, int](), directories: initTable[string, DirectoryRef]())
    elif parts[0] != "$": 
      try:
        let size = parseInt(parts[0])
        let name = parts[1]
        currentDir.files[name] = size
      except ValueError:
        discard 

  discard calculateSizeAndSum(root) 
  echo totalSumLessThan100k

when isMainModule:
  main()
