
def simulateLanternfishGrowth(String filename, int days) {
    // Read the input from the file
    def input = new File(filename).text.trim()
    def fishTimers = input.split(',').collect { it.toInteger() }

    // Create an array to count the number of fish at each timer state (0-8)
    def fishCount = new long[9]
    fishTimers.each { fishCount[it]++ }

    // Simulate the growth over the specified number of days
    for (int day = 0; day < days; day++) {
        // Store the number of fish that will spawn today
        long spawningFish = fishCount[0]

        // Shift the fish timers down
        for (int i = 0; i < 8; i++) {
            fishCount[i] = fishCount[i + 1]
        }

        // Reset the timer for spawning fish and add new fish
        fishCount[6] += spawningFish // Reset timer for existing fish
        fishCount[8] = spawningFish // New fish with timer 8
    }

    // Calculate the total number of fish
    long totalFish = fishCount.sum()
    return totalFish
}

// Main execution
def totalFishAfter256Days = simulateLanternfishGrowth('input.txt', 256)
println "Total lanternfish after 256 days: $totalFishAfter256Days"
