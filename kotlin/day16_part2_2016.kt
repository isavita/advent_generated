import java.io.File

fun main(args: Array<String>) {
    val initialState = readInitialState("input.txt")
    val diskLength = 35651584
    val data = generateData(initialState, diskLength)
    val checksum = calculateChecksum(data)
    println("Checksum: $checksum")
}

fun readInitialState(filename: String): String {
    return File(filename).readLines()[0]
}

fun generateData(initialState: String, length: Int): String {
    var data = initialState
    while (data.length < length) {
        val b = StringBuilder()
        for (i in data.length - 1 downTo 0) {
            if (data[i] == '0') {
                b.append('1')
            } else {
                b.append('0')
            }
        }
        data += "0" + b.toString()
    }
    return data.substring(0, length)
}

fun calculateChecksum(data: String): String {
    var newData = data
    while (newData.length % 2 == 0) {
        val b = StringBuilder()
        for (i in 0 until newData.length step 2) {
            if (newData[i] == newData[i + 1]) {
                b.append('1')
            } else {
                b.append('0')
            }
        }
        newData = b.toString()
    }
    return newData
}