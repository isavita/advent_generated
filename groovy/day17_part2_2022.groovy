
import java.nio.file.Files
import java.nio.file.Paths

class PyroclasticFlow {

    // Define the 5 rock shapes relative to their bottom-left corner (0,0)
    // Coordinates are [x, y]
    static final List<Set<List<Integer>>> ROCKS = [
        // ####
        [[0, 0], [1, 0], [2, 0], [3, 0]].toSet(),
        // .#.
        // ###
        // .#.
        [[1, 0], [0, 1], [1, 1], [2, 1], [1, 2]].toSet(),
        // ..#
        // ..#
        // ###
        [[0, 0], [1, 0], [2, 0], [2, 1], [2, 2]].toSet(), // Note: (0,0) is bottom-left of the '#', coordinates relative to it.
        // #
        // #
        // #
        // #
        [[0, 0], [0, 1], [0, 2], [0, 3]].toSet(),
        // ##
        // ##
        [[0, 0], [1, 0], [0, 1], [1, 1]].toSet()
    ].collect { it as Set<List<Integer>> } // Ensure immutability and Set type

    static final int CHAMBER_WIDTH = 7

    // Represents the settled rocks in the chamber
    // Using a Set for efficient collision checks
    Set<List<Integer>> chamber = new HashSet<>()
    long currentMaxHeight = 0L
    String jets
    int jetIndex = 0
    int rockIndex = 0

    // --- Simulation State for Cycle Detection ---
    // Map: State -> [rockCount, height]
    Map<List, List<Long>> history = [:]
    long addedHeightFromCycles = 0L // Height added by skipping cycles

    PyroclasticFlow(String jetPattern) {
        this.jets = jetPattern
    }

    // Check if a rock at the given position (px, py) collides with walls or settled rocks
    boolean checkCollision(Set<List<Integer>> rockShape, int px, int py) {
        if (py < 0) return true // Collision with floor

        for (List<Integer> point : rockShape) {
            int x = px + point[0]
            int y = py + point[1]

            // Wall collision
            if (x < 0 || x >= CHAMBER_WIDTH) {
                return true
            }
            // Settled rock collision
            if (chamber.contains([x, y])) {
                return true
            }
            // Floor collision (redundant with py < 0 check, but safe)
            if (y < 0) {
                 return true;
            }
        }
        return false
    }

    // Represents the top surface profile relative to the max height
    // Used as part of the state for cycle detection
    // We only need a limited depth to capture the relevant shape
    List<Integer> getTopProfile(int depth) {
        List<Integer> profile = new ArrayList<>(Collections.nCopies(CHAMBER_WIDTH, (int)depth + 1)); // Initialize with large depth
        for (int x = 0; x < CHAMBER_WIDTH; x++) {
            for (int dy = 0; dy <= depth; dy++) {
                 long y = currentMaxHeight - dy;
                 if (y < 0) break; // Don't check below floor
                 if (chamber.contains([x, (int)y])) {
                      profile[x] = dy;
                      break;
                 }
            }
       }
       return profile;
    }


    // Simulate the fall of a single rock
    void simulateOneRock() {
        Set<List<Integer>> currentRockShape = ROCKS[rockIndex]
        rockIndex = (rockIndex + 1) % ROCKS.size()

        // Initial position: left edge 2 units from left wall, bottom edge 3 units above highest rock/floor
        int rockX = 2
        // Note: currentMaxHeight is the y-coordinate of the highest *occupied* cell + 1.
        // So, the highest rock is at y = currentMaxHeight - 1.
        // The new rock's bottom edge should be at y = currentMaxHeight + 3.
        long rockY = currentMaxHeight + 3

        // Rock fall loop
        while (true) {
            // 1. Jet push
            char jet = jets.charAt(jetIndex)
            jetIndex = (jetIndex + 1) % jets.length()
            int dx = (jet == '<') ? -1 : 1
            int nextX = rockX + dx

            if (!checkCollision(currentRockShape, nextX, (int)rockY)) {
                rockX = nextX // Move if no collision
            }

            // 2. Fall down
            long nextY = rockY - 1
            if (!checkCollision(currentRockShape, rockX, (int)nextY)) {
                rockY = nextY // Move down if no collision
            } else {
                // Collision below: Rock comes to rest
                for (List<Integer> point : currentRockShape) {
                    int finalX = rockX + point[0]
                    long finalY = rockY + point[1]
                    chamber.add([finalX, (int)finalY])
                    currentMaxHeight = Math.max(currentMaxHeight, finalY + 1) // Update max height (height is 1 more than max y-coord)
                }
                break // Stop falling
            }
        }
    }

    // Simulate rocks falling and calculate height
    long simulate(long totalRocks) {
        long rockCount = 0L

        while (rockCount < totalRocks) {
            // --- Cycle Detection ---
            // Define state: (next rock index, next jet index, top surface profile)
            // Use a reasonable depth for the profile (e.g., 30-50 units)
             int profileDepth = 30; // Heuristic depth, seems sufficient for this problem
             List stateKey = [rockIndex, jetIndex, getTopProfile(profileDepth)]


            if (addedHeightFromCycles == 0L && history.containsKey(stateKey)) { // Found a cycle and haven't already fast-forwarded
                List<Long> previousState = history[stateKey]
                long prevRockCount = previousState[0]
                long prevHeight = previousState[1]

                long cycleLenRocks = rockCount - prevRockCount
                long cycleHeightGain = currentMaxHeight - prevHeight

                long remainingRocks = totalRocks - rockCount
                long numCycles = remainingRocks / cycleLenRocks

                // Fast forward using the cycles
                addedHeightFromCycles = numCycles * cycleHeightGain
                rockCount += numCycles * cycleLenRocks

                // Important: Clear history after using it to skip,
                // prevents re-detecting the same cycle pattern incorrectly later
                // if the simulation continues for the remainder.
                history.clear()

                // If we skipped exactly to the end, we are done
                 if (rockCount == totalRocks) {
                     break;
                 }
                 // Continue simulating the remaining rocks normally after the skip

            } else if (addedHeightFromCycles == 0L) {
                 // Store state only if we are not already in the post-cycle phase
                 history[stateKey] = [rockCount, currentMaxHeight]
             }

            // Simulate the next rock if we haven't reached the total yet
             if (rockCount < totalRocks) {
                 simulateOneRock()
                 rockCount++
             }
        }

        return currentMaxHeight + addedHeightFromCycles
    }

    // Main entry point
    static void main(String[] args) {
        // Read jet pattern from input.txt
        String jetPattern = Files.readString(Paths.get("input.txt")).trim()

        // --- Part 1 ---
        PyroclasticFlow sim1 = new PyroclasticFlow(jetPattern)
        long heightPart1 = sim1.simulate(2022L)
        println "Part 1: Tower height after 2022 rocks = ${heightPart1}"

        // --- Part 2 ---
        PyroclasticFlow sim2 = new PyroclasticFlow(jetPattern)
        long targetRocksPart2 = 1_000_000_000_000L
        long heightPart2 = sim2.simulate(targetRocksPart2)
        println "Part 2: Tower height after ${targetRocksPart2} rocks = ${heightPart2}"
    }
}
