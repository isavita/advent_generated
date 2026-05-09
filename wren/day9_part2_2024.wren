
import "io" for File

var FreeSpaceMarker = -1

var findLeftmostFit = Fn.new {|disk, file|
  var startPos = file["startPos"]
  var size = file["size"]
  for (i in 0...startPos) {
    if (disk[i] == FreeSpaceMarker) {
      var fits = true
      for (j in 1...size) {
        var idx = i + j
        if (idx >= disk.count || disk[idx] != FreeSpaceMarker) {
          fits = false
          break
        }
      }
      if (fits) return i
    }
  }
  return -1
}

var compactFiles = Fn.new {|disk, files|
  var i = files.count - 1
  while (i >= 0) {
    var file = files[i]
    var targetPos = findLeftmostFit.call(disk, file)
    if (targetPos != -1) {
      var id = file["id"]
      var size = file["size"]
      var oldStart = file["startPos"]
      for (k in 0...size) {
        disk[targetPos + k] = id
      }
      for (k in 0...size) {
        disk[oldStart + k] = FreeSpaceMarker
      }
      file["startPos"] = targetPos
    }
    i = i - 1
  }
}

var calculateChecksum = Fn.new {|disk|
  var sum = 0
  for (i in 0...disk.count) {
    var blockId = disk[i]
    if (blockId != FreeSpaceMarker) {
      sum = sum + i * blockId
    }
  }
  return sum
}

var inputContent = File.read("input.txt").trim()
var disk = []
var files = []
var currentPos = 0
var fileId = 0
var isFileLength = true

for (c in inputContent) {
  var length = Num.fromString(c)
  if (length == 0) {
    isFileLength = !isFileLength
    continue
  }
  if (isFileLength) {
    files.add({
      "id": fileId,
      "size": length,
      "startPos": currentPos
    })
    for (k in 0...length) {
      disk.add(fileId)
    }
    fileId = fileId + 1
  } else {
    for (k in 0...length) {
      disk.add(FreeSpaceMarker)
    }
  }
  currentPos = currentPos + length
  isFileLength = !isFileLength
}

compactFiles.call(disk, files)
var checksum = calculateChecksum.call(disk)
System.print(checksum)
