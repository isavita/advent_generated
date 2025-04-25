
import java.util.PriorityQueue
import java.util.Queue
import java.util.LinkedList

@groovy.transform.ToString(includeNames = true, includeFields = true)
class State implements Comparable<State> {
    List<List<String>> grid
    int energyUsed
    // String path // Optional: for debugging, removed for conciseness

    State(List<List<String>> grid, int energyUsed = 0) { //, String path = "") {
        this.grid = grid
        this.energyUsed = energyUsed
        // this.path = path
    }

    @Override
    int compareTo(State other) {
        return this.energyUsed <=> other.energyUsed
    }

    int getValue() {
        return energyUsed
    }

    State copy() {
        def newGrid = grid.collect { row -> row.clone() }
        return new State(newGrid, energyUsed) //, path)
    }

    String gridKey() {
         grid.collect { it.join('') }.join('\n')
    }

    boolean allDone(Map<List<Integer>, String> roomCoordToWantChar) {
        return roomCoordToWantChar.every { coord, want ->
            grid[coord[0]][coord[1]] == want
        }
    }

    List<List<Integer>> getUnsettledCoords(Map<List<Integer>, String> roomCoordToWantChar) {
        def unsettled = [] as List<List<Integer>>
        def amphipods = ['A', 'B', 'C', 'D'] as Set<String>

        // Check hallway
        (1..<grid[0].size() - 1).each { col ->
            if (grid[1][col] in amphipods) {
                unsettled << [1, col]
            }
        }

        // Check rooms
        [3, 5, 7, 9].each { col ->
            boolean roomFullFromBack = true
            for (int row = grid.size() - 2; row >= 2; row--) {
                def coord = [row, col]
                def wantChar = roomCoordToWantChar[coord]
                def gotChar = grid[row][col]
                if (gotChar != '.') {
                    if (gotChar != wantChar) {
                        roomFullFromBack = false
                        unsettled << coord
                    } else if (gotChar == wantChar && !roomFullFromBack) {
                        unsettled << coord
                    }
                } else {
                     // If we find an empty spot, things above it are not settled correctly relative to it
                     roomFullFromBack = false
                }
            }
        }
        return unsettled
    }


    List<List<Integer>> getNextPossibleMoves(List<Integer> unsettledCoord, Map<List<Integer>, String> roomCoordToWantChar, Set<List<Integer>> coordsInFrontOfRooms) {
        def (ur, uc) = unsettledCoord
        def unsettledChar = grid[ur][uc]
        def possible = [] as List<List<Integer>>
        boolean startedInHallway = (ur == 1)

        Queue<List<Integer>> queue = new LinkedList<>()
        queue.add(unsettledCoord)
        def seen = [unsettledCoord] as Set<List<Integer>>
        def distances = [(unsettledCoord): 0] as Map<List<Integer>, Integer>

        while (!queue.isEmpty()) {
            def front = queue.poll()
            def (fr, fc) = front
            def currentDist = distances[front]

            if (front != unsettledCoord) {
                 boolean isFrontOfRoom = front in coordsInFrontOfRooms

                if (!isFrontOfRoom) { // Cannot stop in front of a room
                    def wantChar = roomCoordToWantChar[front]
                    boolean isRoomCoord = (wantChar != null)

                    if (!isRoomCoord) { // Hallway spot (not in front of room)
                        if (!startedInHallway) { // Can only move to hallway if started in room
                            possible << front
                        }
                    } else { // Room spot
                        if (wantChar == unsettledChar) { // Is it the correct room?
                           // Check if the path to the room is clear and if deeper spots are occupied correctly
                           boolean deeperSpotsOk = true
                           boolean canEnterRoom = true
                           for(int r = fr + 1; r < grid.size() - 1; r++) {
                               def deeperCoord = [r, fc]
                               if (grid[r][fc] == '.') { // Cannot move over empty space
                                    canEnterRoom = false
                                    break
                               }
                               if (grid[r][fc] != wantChar) { // Cannot move if wrong amphipod is deeper
                                   canEnterRoom = false
                                   break
                               }
                           }

                           if (canEnterRoom) {
                                possible << front // Can move into the correct room
                           }
                        }
                    }
                }
            }

            // Explore neighbors
            [[-1, 0], [1, 0], [0, -1], [0, 1]].each { dr, dc ->
                def next = [fr + dr, fc + dc]
                if (grid[next[0]][next[1]] == '.' && !(next in seen)) {
                    seen.add(next)
                    distances[next] = currentDist + 1
                    queue.add(next)
                }
            }
        }

        // If we started in the hallway, we *must* move into a room, filter others
        if (startedInHallway) {
             possible = possible.findAll { coord -> roomCoordToWantChar.containsKey(coord) }
        }

        return possible
    }

    static int calcEnergy(String charType, List<Integer> start, List<Integer> end) {
        def energyPerType = ['A': 1, 'B': 10, 'C': 100, 'D': 1000]
        def dist = Math.abs(end[1] - start[1]) + Math.abs(end[0] - start[0]) // Manhattan distance works here

        // Correct distance calc as per Go (move out, across, move in)
        dist = Math.abs(end[1] - start[1]) // Horizontal
        dist += start[0] - 1             // Steps out of start row
        dist += end[0] - 1               // Steps into end row

        return energyPerType[charType] * dist
    }

}

// --- Main Script ---

def input = new File('input.txt').text.trim()

def grid = input.split('\n').collect { line -> line.split('') as List<String> }
def startState = new State(grid)

// Define room coordinates and desired amphipod type
// Adjust based on input (Part 1 has 2 rows in room, Part 2 has 4)
def roomCoordToWantChar = [:] as Map<List<Integer>, String>
def roomDepth = grid.size() - 3 // Number of spots inside each room (2 for part 1)
(0..<roomDepth).each{ i ->
    def row = 2 + i
    roomCoordToWantChar[[row, 3]] = 'A'
    roomCoordToWantChar[[row, 5]] = 'B'
    roomCoordToWantChar[[row, 7]] = 'C'
    roomCoordToWantChar[[row, 9]] = 'D'
}


def coordsInFrontOfRooms = [[1, 3], [1, 5], [1, 7], [1, 9]] as Set<List<Integer>>

PriorityQueue<State> minHeap = new PriorityQueue<>()
minHeap.add(startState)
Set<String> seenGrids = new HashSet<>()

int finalEnergy = -1

while (!minHeap.isEmpty()) {
    State current = minHeap.poll()
    String key = current.gridKey()

    if (key in seenGrids) {
        continue
    }
    seenGrids.add(key)

    if (current.allDone(roomCoordToWantChar)) {
        finalEnergy = current.energyUsed
        break
    }

    def unsettledCoords = current.getUnsettledCoords(roomCoordToWantChar)

    unsettledCoords.each { unsettledCoord ->
        def (ur, uc) = unsettledCoord
        def possibleMoves = current.getNextPossibleMoves(unsettledCoord, roomCoordToWantChar, coordsInFrontOfRooms)

        possibleMoves.each { nextCoord ->
             def (nr, nc) = nextCoord

             State nextState = current.copy()
             String mover = nextState.grid[ur][uc]

             nextState.energyUsed += State.calcEnergy(mover, unsettledCoord, nextCoord)
             // Optional: Update path for debugging
             // nextState.path += "${mover}${unsettledCoord}->${nextCoord}{${nextState.energyUsed}},"
             nextState.grid[nr][nc] = mover
             nextState.grid[ur][uc] = '.'

             minHeap.add(nextState)
        }
    }
}

println finalEnergy
