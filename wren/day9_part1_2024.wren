
import "io" for File

var FreeSpaceMarker = -1

var main = Fn.new {
  var input = File.read("input.txt").trim()
  if (input.isEmpty) {
    System.print("Error: Input file is empty or not found.")
    return
  }

  var disk = []
  var fileId = 0
  var isFile = true

  for (ch in input) {
    var length = Num.fromString(ch)
    if (isFile) {
      if (length > 0) {
        for (i in 0...length) disk.add(fileId)
        fileId = fileId + 1
      }
    } else {
      if (length > 0) {
        for (i in 0...length) disk.add(FreeSpaceMarker)
      }
    }
    isFile = !isFile
  }

  var left = 0
  var right = disk.count - 1

  while (true) {
    while (left < disk.count && disk[left] != FreeSpaceMarker) left = left + 1
    while (right >= 0 && disk[right] == FreeSpaceMarker) right = right - 1
    if (left >= right) break
    disk[left] = disk[right]
    disk[right] = FreeSpaceMarker
  }

  var checksum = 0
  for (i in 0...disk.count) {
    var block = disk[i]
    if (block != FreeSpaceMarker) checksum = checksum + i * block
  }

  System.print(checksum)
}

main.call()
