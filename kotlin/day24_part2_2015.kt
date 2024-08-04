import java.io.File
import java.lang.Integer.max
import java.lang.Integer.min
import kotlin.math.max
import kotlin.math.min

fun main() {
    val lines = File("input.txt").readLines()
    val packages = lines.map { it.toInt() }
    val totalWeight = packages.sum()
    val targetWeight = totalWeight / 4
    var bestQE = Int.MAX_VALUE
    var bestLength = Int.MAX_VALUE

    for (comb in 1 until (1 shl packages.size)) {
        var groupWeight = 0
        var qe = 1
        var groupLength = 0
        for (i in 0 until packages.size) {
            if (comb and (1 shl i) != 0) {
                groupWeight += packages[i]
                qe *= packages[i]
                groupLength++
            }
        }
        if (groupWeight == targetWeight && groupLength <= bestLength) {
            if (groupLength < bestLength || qe < bestQE) {
                if (canSplit(packages, comb, targetWeight)) {
                    bestLength = groupLength
                    bestQE = qe
                }
            }
        }
    }

    println(bestQE)
}

fun canSplit(packages: List<Int>, firstGroupComb: Int, targetWeight: Int): Boolean {
    val remainingPackages = mutableListOf<Int>()
    for (i in 0 until packages.size) {
        if (firstGroupComb and (1 shl i) == 0) {
            remainingPackages.add(packages[i])
        }
    }
    for (comb1 in 1 until (1 shl remainingPackages.size)) {
        var group1Weight = 0
        for (i in 0 until remainingPackages.size) {
            if (comb1 and (1 shl i) != 0) {
                group1Weight += remainingPackages[i]
            }
        }
        if (group1Weight == targetWeight) {
            for (comb2 in 1 until (1 shl remainingPackages.size)) {
                if (comb1 and comb2 == 0) {
                    var group2Weight = 0
                    for (i in 0 until remainingPackages.size) {
                        if (comb2 and (1 shl i) != 0) {
                            group2Weight += remainingPackages[i]
                        }
                    }
                    if (group2Weight == targetWeight) {
                        return true
                    }
                }
            }
        }
    }
    return false
}