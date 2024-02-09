import java.io.File

data class Mirror(val rows: List<Int>, val cols: List<Int>)

fun parseInput(input: List<String>): List<Mirror> {
    val mirrors = mutableListOf<Mirror>()
    var mirrorStr = mutableListOf<String>()

    for (line in input) {
        if (line == "") {
            mirrors.add(parseMirror(mirrorStr))
            mirrorStr = mutableListOf()
        } else {
            mirrorStr.add(line)
        }
    }
    mirrors.add(parseMirror(mirrorStr))

    return mirrors
}

fun parseMirror(mirrorStr: List<String>): Mirror {
    val rows = mutableListOf<Int>()
    val cols = MutableList(mirrorStr[0].length) { 0 }

    for (y in mirrorStr.indices) {
        var row = 0
        for ((x, char) in mirrorStr[y].withIndex()) {
            row = row shl 1
            cols[x] = cols[x] shl 1
            if (char == '#') {
                row++
                cols[x]++
            }
        }
        rows.add(row)
    }

    return Mirror(rows, cols)
}

fun getMirrorAxis(lines: List<Int>): Int {
    for (i in 1 until lines.size) {
        var isMirror = true

        for (j in 0 until minOf(i, lines.size - i)) {
            if (lines[i - 1 - j] != lines[i + j]) {
                isMirror = false
            }
        }

        if (isMirror) {
            return i
        }
    }

    return 0
}

fun getMirrorAxisWithOneSmudge(lines: List<Int>): Int {
    for (i in 1 until lines.size) {
        var isMirror = true
        var numSmudges = 0

        for (j in 0 until minOf(i, lines.size - i)) {
            if (lines[i - 1 - j] != lines[i + j]) {
                if (numSmudges > 0) {
                    isMirror = false
                } else {
                    val dif = lines[i - 1 - j] xor lines[i + j]
                    val isOnlyOneSmudge = (dif and (dif - 1)) == 0
                    if (isOnlyOneSmudge) {
                        numSmudges++
                    } else {
                        isMirror = false
                    }
                }
            }
        }

        if (isMirror && numSmudges == 1) {
            return i
        }
    }

    return 0
}

fun solve(input: List<String>): Int {
    val mirrors = parseInput(input)

    var res = 0
    for (mirror in mirrors) {
        res += getMirrorAxisWithOneSmudge(mirror.cols)
        res += getMirrorAxisWithOneSmudge(mirror.rows) * 100
    }
    return res
}

fun main(args: Array<String>) {
    val input = File("input.txt").readLines()
    println(solve(input))
}