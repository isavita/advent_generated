import java.io.File

fun main(args: Array<String>) {
    val messages = mutableListOf<String>()
    File("input.txt").forEachLine { messages.add(it) }

    val originalMessage = getOriginalMessage(messages)
    println(originalMessage)
}

fun getOriginalMessage(messages: List<String>): String {
    if (messages.isEmpty()) {
        return ""
    }
    val messageLength = messages[0].length
    val count = Array(messageLength) { mutableMapOf<Char, Int>() }

    messages.forEach { message ->
        message.forEachIndexed { index, char ->
            count[index][char] = count[index].getOrDefault(char, 0) + 1
        }
    }

    val originalMessage = StringBuilder()
    count.forEach { charCount ->
        originalMessage.append(getLeastCommonChar(charCount))
    }

    return originalMessage.toString()
}

fun getLeastCommonChar(count: Map<Char, Int>): Char {
    var minChar = ' '
    var minCount = Int.MAX_VALUE
    count.forEach { (char, cnt) ->
        if (cnt < minCount) {
            minCount = cnt
            minChar = char
        }
    }
    return minChar
}