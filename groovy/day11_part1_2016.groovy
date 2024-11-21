
import java.util.*

class State {
    int[] floor = [0, 0, 0, 0] // Represents the items on each floor.  Bitmask: 1=generator, 2=microchip
    int elevator = 1 // Elevator floor (1-4)

    @Override
    boolean equals(Object o) {
        if (this.is(o)) return true
        if (getClass() != o.getClass()) return false
        State state = (State) o
        return Arrays.equals(floor, state.floor) && elevator == state.elevator
    }

    @Override
    int hashCode() {
        int result = Arrays.hashCode(floor)
        result = 31 * result + elevator
        return result
    }

    String toString() {
        "F4: ${floor[3]}\nF3: ${floor[2]}\nF2: ${floor[1]}\nF1: ${floor[0]} E"
    }

    boolean isSafe() {
        for (int i = 0; i < 4; i++) {
            int generators = floor[i] & 0x1111
            int microchips = floor[i] & 0x2222
            if (generators > 0 && microchips > generators) return false
        }
        return true
    }

    boolean isSolved() {
        return floor[0] == 0 && floor[1] == 0 && floor[2] == 0
    }
}

def solve() {
    def initialState = new State()
    initialState.floor[0] = 0b0011 // HM LM on floor 1
    initialState.floor[1] = 0b0001 // HG on floor 2
    initialState.floor[2] = 0b0010 // LG on floor 3

    Queue<Pair<State, Integer>> queue = new LinkedList<>()
    queue.add(new Pair<>(initialState, 0))

    Set<State> visited = new HashSet<>()
    visited.add(initialState)

    while (!queue.isEmpty()) {
        Pair<State, Integer> current = queue.poll()
        State state = current.getKey()
        int steps = current.getValue()

        if (state.isSolved()) return steps

        for (int i = 0; i < 2; i++) { // Iterate through possible item combinations (0-3)
            for (int j = 0; j < 2; j++) {
                int item1 = i
                int item2 = j
                if (item1 == item2 && item1 == 0) continue // Skip no items

                int nextFloor = state.elevator + (item1 == 0 ? 0 : (item1 == 1 ? 1 : -1))
                if (nextFloor < 1 || nextFloor > 4) continue

                State nextState = new State()
                nextState.elevator = nextFloor
                for (int k = 0; k < 4; k++) {
                    nextState.floor[k] = state.floor[k]
                }

                int itemMask1 = (item1 == 0 ? 0 : (item1 == 1 ? 1 : 2)) << (item1 == 0 ? 0 : (item1 == 1 ? 4 : 8))
                int itemMask2 = (item2 == 0 ? 0 : (item2 == 1 ? 1 : 2)) << (item2 == 0 ? 0 : (item2 == 1 ? 4 : 8))

                nextState.floor[state.elevator - 1] -= (itemMask1 | itemMask2)
                nextState.floor[nextFloor - 1] += (itemMask1 | itemMask2)

                if (nextState.isSafe() && !visited.contains(nextState)) {
                    visited.add(nextState)
                    queue.add(new Pair<>(nextState, steps + 1))
                }
            }
        }
    }
    return -1 // Should not reach here if a solution exists
}

println solve()
