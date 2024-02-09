import java.io.File

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()
    var head: Star? = null
    var tail: Star? = null
    val re = Regex("position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>")
    for (line in lines) {
        val split = re.find(line)?.destructured?.toList()
        if (split?.size != 4) {
            continue
        }
        val star = Star(
            split[0].toInt(),
            split[1].toInt(),
            split[2].toInt(),
            split[3].toInt()
        )
        if (head == null) {
            head = star
            tail = star
        } else {
            tail?.next = star
            tail = star
        }
    }

    var smallestT = 0
    var smallestArea = Int.MAX_VALUE
    for (t in 1 until 100000) {
        var maxX = 0
        var maxY = 0
        var minX = 0
        var minY = 0

        var temp = head?.next
        while (temp?.next != null) {
            val x = temp.x + temp.vX * t
            if (maxX < x) {
                maxX = x
            } else if (minX > x) {
                minX = x
            }
            val y = temp.y + temp.vY * t
            if (maxY < y) {
                maxY = y
            } else if (minY > y) {
                minY = y
            }
            temp = temp.next
        }

        val lenX = maxX - minY + 1
        val lenY = maxY - minY + 1
        val area = lenX + lenY

        if (smallestArea > area) {
            smallestArea = area
            smallestT = t
        }
    }
    println(smallestT)

    val t = smallestT

    var maxX = 0
    var maxY = 0
    var minX = 0
    var minY = 0

    var temp = head?.next
    while (temp?.next != null) {
        temp.x += temp.vX * t
        if (maxX < temp.x) {
            maxX = temp.x
        } else if (minX > temp.x) {
            minX = temp.x
        }
        temp.y += temp.vY * t
        if (maxY < temp.y) {
            maxY = temp.y
        } else if (minY > temp.y) {
            minY = temp.y
        }
        temp = temp.next
    }

    val mapper = Array(maxY - minY + 1) { BooleanArray(maxX - minX + 1) }

    temp = head?.next
    while (temp?.next != null) {
        mapper[temp.y][temp.x] = true
        temp = temp.next
    }

    for (i in mapper.indices) {
        for (j in mapper[0].indices) {
        }
    }
}

data class Star(
    var x: Int,
    var y: Int,
    var vX: Int,
    var vY: Int,
    var next: Star? = null
)