import java.io.File

fun main(args: Array<String>) {
    val messages = mutableListOf<String>()
    File("input.txt").forEachLine { messages.add(it) }

    val correctedMessage = getCorrectedMessage(messages)
    println(correctedMessage)
}

fun getCorrectedMessage(messages: List<String>): String {
    if (messages.isEmpty()) {
        return ""
    }
    val messageLength = messages[0].length
    val count = List(messageLength) { mutableMapOf<Char, Int>() }

    for (message in messages) {
        message.forEachIndexed { index, char ->
            count[index][char] = count[index].getOrDefault(char, 0) + 1
        }
    }

    val correctedMessage = StringBuilder()
    count.forEach { charCount ->
        correctedMessage.append(getMostCommonChar(charCount))
    }

    return correctedMessage.toString()
}

fun getMostCommonChar(count: Map<Char, Int>): Char {
    var maxChar: Char = ' '
    var maxCount = 0
    count.forEach { (char, cnt) ->
        if (cnt > maxCount) {
            maxCount = cnt
            maxChar = char
        }
    }
    return maxChar
}