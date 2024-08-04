import java.io.File
import java.lang.Exception

fun main() {
    val input = File("input.txt").readText().trim()
    val ans = jumbledSevenSegment(input)
    println(ans)
}

fun jumbledSevenSegment(input: String): Int {
    val parsedInput = mutableListOf<List<String>>()
    for ((i, line) in input.split("\n").withIndex()) {
        val parts = Regex("([a-g]+)").findAll(line).map { it.value }.toList()
        if (parts.size != 14) throw Exception("should be 14 parts in each input line, got ${parts.size} for line $i")
        val fourteen = parts.map { alphabetizeString(it) }
        parsedInput.add(fourteen)
    }

    var ans = 0
    val indexToCharacters = Array(10) { "" }
    for (set in parsedInput) {
        var workingSet = set.take(10)
        val killIndices = mutableListOf<Int>()

        for ((i, mapping) in workingSet.withIndex()) {
            when (mapping.length) {
                2 -> {
                    indexToCharacters[1] = mapping
                    killIndices.add(i)
                }
                4 -> {
                    indexToCharacters[4] = mapping
                    killIndices.add(i)
                }
                3 -> {
                    indexToCharacters[7] = mapping
                    killIndices.add(i)
                }
                7 -> {
                    indexToCharacters[8] = mapping
                    killIndices.add(i)
                }
            }
        }

        workingSet = removeSliceIndices(workingSet, killIndices)

        val zeroThreeOrNine = mutableListOf<String>()
        killIndices.clear()
        for ((i, mapping) in workingSet.withIndex()) {
            if (checkStringOverlap(mapping, indexToCharacters[1])) {
                zeroThreeOrNine.add(mapping)
                killIndices.add(i)
            }
        }
        if (zeroThreeOrNine.size != 3) throw Exception("one three or nine does not have three matches: got ${zeroThreeOrNine.size}")

        val iterator = zeroThreeOrNine.iterator()
        while (iterator.hasNext()) {
            val maybe039 = iterator.next()
            if (maybe039.length == 5) {
                indexToCharacters[3] = maybe039
                iterator.remove()
                break
            }
        }

        val iterator2 = zeroThreeOrNine.iterator()
        while (iterator2.hasNext()) {
            val maybe09 = iterator2.next()
            if (checkStringOverlap(maybe09, indexToCharacters[4])) {
                indexToCharacters[9] = maybe09
                iterator2.remove()
            }
        }

        indexToCharacters[0] = zeroThreeOrNine[0]

        workingSet = removeSliceIndices(workingSet, killIndices)
        if (workingSet.size != 3) throw Exception("expected length of 3 at this stage, got ${workingSet.size}")

        for ((i, mapping) in workingSet.withIndex()) {
            if (mapping.length == 6) {
                indexToCharacters[6] = mapping
                workingSet = removeSliceIndices(workingSet, listOf(i))
            }
        }

        for ((i, mapping) in workingSet.withIndex()) {
            if (checkStringOverlap(indexToCharacters[9], mapping)) {
                indexToCharacters[5] = mapping
                workingSet = removeSliceIndices(workingSet, listOf(i))
            }
        }

        if (workingSet.size != 1) throw Exception("expected length of 1 at this stage, got ${workingSet.size}")

        indexToCharacters[2] = workingSet[0]

        var num = 0
        for (out in set.takeLast(4)) {
            for ((i, mapping) in indexToCharacters.withIndex()) {
                if (out == mapping) {
                    num *= 10
                    num += i
                }
            }
        }
        ans += num
    }

    return ans
}

fun removeSliceIndices(sli: List<String>, indices: List<Int>): List<String> {
    val m = indices.toSet()
    return sli.filterIndexed { i, _ -> i !in m }
}

fun checkStringOverlap(larger: String, smaller: String): Boolean {
    if (larger.length < smaller.length) return checkStringOverlap(smaller, larger)
    val largeMap = larger.toSet()
    return smaller.all { it in largeMap }
}

fun alphabetizeString(input: String): String {
    return input.toCharArray().sorted().joinToString("")
}