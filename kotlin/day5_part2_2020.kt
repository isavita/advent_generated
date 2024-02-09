import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val seatIDs = mutableListOf<Int>()

    file.forEachLine {
        var pass = it
        pass = pass.replace("F", "0").replace("B", "1").replace("L", "0").replace("R", "1")
        val seatID = decode(pass)
        seatIDs.add(seatID)
    }

    seatIDs.sort()

    for (i in 0 until seatIDs.size - 1) {
        if (seatIDs[i + 1] != seatIDs[i] + 1) {
            println(seatIDs[i] + 1)
            break
        }
    }
}

fun decode(pass: String): Int {
    val row = binaryToInt(pass.substring(0, 7))
    val column = binaryToInt(pass.substring(7))
    return row * 8 + column
}

fun binaryToInt(binaryStr: String): Int {
    var result = 0
    for (i in binaryStr.indices) {
        if (binaryStr[i] == '1') {
            result = result or (1 shl (binaryStr.length - i - 1))
        }
    }
    return result
}