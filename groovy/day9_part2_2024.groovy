class Main {
  static class FileSegment { int id; int start; int end }

  static void main(String[] args) {
    def inputFile = new File("input.txt")
    if (!inputFile.exists()) {
      println 0
      return
    }
    def lines = inputFile.readLines()
    if (lines.isEmpty()) {
      println 0
      return
    }
    def line = lines[0].trim()
    def disk = []
    int fileIdCounter = 0
    boolean isFileSegment = true

    def chars = line.toCharArray()
    for (int idx = 0; idx < chars.length; idx++) {
      int length = Character.getNumericValue(chars[idx])
      for (int k = 0; k < length; k++) disk << (isFileSegment ? fileIdCounter : -1)
      if (isFileSegment) fileIdCounter++
      isFileSegment = !isFileSegment
    }

    def files = []
    int currentSegmentId = -2
    int segmentStartIdx = 0

    for (int i = 0; i < disk.size(); i++) {
      int value = disk[i]
      if (value == -1) {
        currentSegmentId = -2
        continue
      }
      if (value != currentSegmentId) {
        currentSegmentId = value
        segmentStartIdx = i
      }
      if (i == disk.size() - 1 || disk[i + 1] != value) {
        files << new FileSegment(id: value, start: segmentStartIdx, end: i)
      }
    }

    files.sort { a, b -> b.id <=> a.id }

    for (f in files) {
      int fileLen = f.end - f.start + 1
      int leftmostFreeSpanStart = -1
      int currentFreeSpanLen = 0
      for (int i = 0; i < f.start; i++) {
        if (disk[i] == -1) {
          if (currentFreeSpanLen == 0) leftmostFreeSpanStart = i
          currentFreeSpanLen++
          if (currentFreeSpanLen == fileLen) break
        } else {
          currentFreeSpanLen = 0
          leftmostFreeSpanStart = -1
        }
      }
      if (leftmostFreeSpanStart != -1 && currentFreeSpanLen == fileLen) {
        for (int i = f.start; i <= f.end; i++) disk[i] = -1
        for (int i = 0; i < fileLen; i++) disk[leftmostFreeSpanStart + i] = f.id
      }
    }

    long checksum = 0L
    for (int i = 0; i < disk.size(); i++) {
      int val = disk[i]
      if (val != -1) checksum += (long) i * val
    }

    println checksum
  }
}
Main.main(null)