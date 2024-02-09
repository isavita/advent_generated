import java.io.File
import java.security.MessageDigest

data class Point(val x: Int, val y: Int, val path: String)

fun main(args: Array<String>) {
    val passcode = readPasscode("input.txt")
    val longestPathLength = findLongestPathLength(passcode)
    println(longestPathLength)
}

fun readPasscode(filename: String): String {
    val file = File(filename)
    return file.readLines()[0]
}

fun findLongestPathLength(passcode: String): Int {
    var longest = 0
    val queue = mutableListOf(Point(0, 0, ""))
    while (queue.isNotEmpty()) {
        val point = queue.removeAt(0)

        if (point.x == 3 && point.y == 3) {
            if (point.path.length > longest) {
                longest = point.path.length
            }
            continue
        }

        for (dir in getOpenDoors(passcode, point.path)) {
            var nextPoint = Point(point.x, point.y, point.path + dir)
            when (dir) {
                "U" -> nextPoint = Point(point.x, point.y - 1, point.path + dir)
                "D" -> nextPoint = Point(point.x, point.y + 1, point.path + dir)
                "L" -> nextPoint = Point(point.x - 1, point.y, point.path + dir)
                "R" -> nextPoint = Point(point.x + 1, point.y, point.path + dir)
            }

            if (nextPoint.x in 0..3 && nextPoint.y in 0..3) {
                queue.add(nextPoint)
            }
        }
    }
    return longest
}

fun getOpenDoors(passcode: String, path: String): List<String> {
    val hash = md5Hash(passcode + path)
    val doors = mutableListOf<String>()
    if (hash[0] in 'b'..'f') {
        doors.add("U")
    }
    if (hash[1] in 'b'..'f') {
        doors.add("D")
    }
    if (hash[2] in 'b'..'f') {
        doors.add("L")
    }
    if (hash[3] in 'b'..'f') {
        doors.add("R")
    }
    return doors
}

fun md5Hash(input: String): String {
    val md = MessageDigest.getInstance("MD5")
    val hashBytes = md.digest(input.toByteArray())
    return hashBytes.joinToString("") { "%02x".format(it) }
}