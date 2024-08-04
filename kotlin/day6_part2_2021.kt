import java.io.File

fun main() {
    val input = File("input.txt").readText().trim().split(",").map { it.toInt() }
    val initialState = LongArray(9) { 0 }

    // Initialize the initial state
    for (timer in input) {
        initialState[timer]++
    }

    // Simulate the growth of lanternfish
    fun simulate(days: Int): Long {
        val state = initialState.copyOf()
        for (day in 0 until days) {
            val newFish = state[0]
            for (i in 0 until 8) {
                state[i] = state[i + 1]
            }
            state[6] += newFish
            state[8] = newFish
        }
        return state.sum()
    }

    // Print the results
    println("Number of lanternfish after 80 days: ${simulate(80)}")
    println("Number of lanternfish after 256 days: ${simulate(256)}")
}