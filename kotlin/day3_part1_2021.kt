import java.io.File

fun main() {
    val input = File("input.txt").readLines()
    val gammaRate = input[0].mapIndexed { index, _ -> input.map { it[index] }.groupBy { it }.maxByOrNull { it.value.size }!!.key }.joinToString("")
    val epsilonRate = input[0].mapIndexed { index, _ -> input.map { it[index] }.groupBy { it }.minByOrNull { it.value.size }!!.key }.joinToString("")
    val powerConsumption = gammaRate.toInt(2) * epsilonRate.toInt(2)
    println(powerConsumption)
}