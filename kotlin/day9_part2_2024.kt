
import java.io.File
import java.io.InputStream

fun main() {
    val inputStream: InputStream = File("input.txt").inputStream()
    val line = inputStream.bufferedReader().use { it.readText() }.trim()

    println("Part 1 Result: ${solvePart1(line)}")
    println("Part 2 Result: ${solvePart2(line)}")
}

fun solvePart1(input: String): Long {
    val disk = parseDisk(input)
    compactDiskPart1(disk)
    return calculateChecksum(disk)
}

fun solvePart2(input: String): Long {
    val disk = parseDisk(input)
    compactDiskPart2(disk)
    return calculateChecksum(disk)
}

fun parseDisk(input: String): MutableList<Pair<Int, Int>> {
    val disk = mutableListOf<Pair<Int, Int>>()
    var fileId = 0
    var i = 0
    while (i < input.length) {
        val length = input[i].digitToInt()
        val isFile = (i % 2 == 0)
        repeat(length) {
            disk.add(Pair(if (isFile) fileId else -1, 0))
        }
        if (isFile) {
            fileId++
        }
        i++
    }
    return disk
}


fun compactDiskPart1(disk: MutableList<Pair<Int, Int>>) {
    var writeIndex = 0
    while (true) {
        // Find next free space
        while (writeIndex < disk.size && disk[writeIndex].first != -1) {
            writeIndex++
        }
        if (writeIndex >= disk.size) break

        // Find next block to move
        var readIndex = disk.size - 1
        while (readIndex >= 0 && (disk[readIndex].first == -1 || readIndex <= writeIndex)) {
            readIndex--
        }
        if (readIndex < 0) break

        // Move the block
        disk[writeIndex] = disk[readIndex]
        disk[readIndex] = Pair(-1, 0)
    }
}

fun compactDiskPart2(disk: MutableList<Pair<Int, Int>>) {
    val fileSizes = mutableMapOf<Int, Int>()
    for ((fileId, _) in disk) {
        if (fileId != -1) {
            fileSizes[fileId] = fileSizes.getOrDefault(fileId, 0) + 1
        }
    }

    val sortedFileIds = fileSizes.keys.sortedDescending()

    for (fileId in sortedFileIds) {
        val fileSize = fileSizes[fileId] ?: continue

        // Find the start index of the file
        var fileStartIndex = -1
        for (i in disk.indices) {
            if (disk[i].first == fileId) {
                fileStartIndex = i
                break
            }
        }
        if(fileStartIndex == -1) continue

        // Find a free space to move the whole file
        var freeSpaceStartIndex = -1
        var currentFreeSpaceSize = 0
        for (i in 0 until fileStartIndex) {
            if (disk[i].first == -1) {
                currentFreeSpaceSize++
                if (currentFreeSpaceSize == fileSize) {
                    freeSpaceStartIndex = i - fileSize + 1
                    break
                }
            } else {
                currentFreeSpaceSize = 0
            }
        }


        // Move the file if free space is found
        if (freeSpaceStartIndex != -1) {

            val tempFileBlocks = mutableListOf<Pair<Int,Int>>()
            for (j in fileStartIndex until fileStartIndex + fileSize){
                tempFileBlocks.add(disk[j])
            }

            for(k in fileStartIndex + fileSize -1 downTo fileStartIndex) {
                disk[k] = Pair(-1, 0)
            }

            for ((index, block) in tempFileBlocks.withIndex()) {
                disk[freeSpaceStartIndex + index] = block
            }


        }
    }
}

fun calculateChecksum(disk: List<Pair<Int, Int>>): Long {
    var checksum = 0L
    for ((index, pair) in disk.withIndex()) {
        if (pair.first != -1) {
            checksum += index.toLong() * pair.first.toLong()
        }
    }
    return checksum
}
