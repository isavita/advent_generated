import java.io.File

data class LLNode(
    val elfNum: Int,
    var presents: Int,
    var next: LLNode? = null
)

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim().toInt()
    val ans = elephant(input)
    println(ans)
}

fun elephant(startingElves: Int): Int {
    var root = LLNode(1, 1)
    var iter = root
    for (i in 2..startingElves) {
        iter.next = LLNode(i, 1)
        iter = iter.next!!
    }
    iter.next = root

    var isOddLength = startingElves % 2 == 1
    var beforeAcross = root
    for (i in 0 until startingElves / 2 - 1) {
        beforeAcross = beforeAcross.next!!
    }

    while (root.next != root) {
        root.presents += beforeAcross.next!!.presents

        beforeAcross.next = beforeAcross.next!!.next

        if (isOddLength) {
            beforeAcross = beforeAcross.next!!
        }
        isOddLength = !isOddLength
        root = root.next!!
    }

    return root.elfNum
}