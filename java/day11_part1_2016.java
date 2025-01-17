
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class RadioisotopeThermoelectricGenerators {

    private static class State {
        int elevator;
        List<Set<Integer>> floors;
        int steps;

        public State(int elevator, List<Set<Integer>> floors, int steps) {
            this.elevator = elevator;
            this.floors = floors;
            this.steps = steps;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            State state = (State) o;
            return elevator == state.elevator &&
                    floors.equals(state.floors);
        }

        @Override
        public int hashCode() {
            return Objects.hash(elevator, floors);
        }

        public boolean isSafe() {
            for (Set<Integer> floor : floors) {
                boolean hasGenerator = false;
                Set<Integer> chipsWithoutGenerators = new HashSet<>();
                for (int item : floor) {
                    if (item > 0) {
                        hasGenerator = true;
                    } else if (!floor.contains(-item)) {
                        chipsWithoutGenerators.add(item);
                    }
                }
                if (hasGenerator && !chipsWithoutGenerators.isEmpty()) {
                    return false;
                }
            }
            return true;
        }

        public boolean isComplete(int numPairs) {
            for (int i = 0; i < 3; i++) {
                if (!floors.get(i).isEmpty()) {
                    return false;
                }
            }
            return floors.get(3).size() == numPairs * 2;
        }
    }

    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            List<Set<Integer>> initialFloors = new ArrayList<>();
            Map<String, Integer> elementMap = new HashMap<>();
            int elementCount = 0;

            String line;
            while ((line = reader.readLine()) != null) {
                Set<Integer> floor = new HashSet<>();
                String[] parts = line.split(" a ");
                for (int i = 1; i < parts.length; i++) {
                    String[] subParts = parts[i].split(" ");
                    String elementName = subParts[0].replace(",", "").replace("-compatible", "");
                    if (!elementMap.containsKey(elementName)) {
                        elementMap.put(elementName, ++elementCount);
                    }
                    int elementId = elementMap.get(elementName);
                    if (subParts[1].startsWith("microchip")) {
                        floor.add(-elementId);
                    } else {
                        floor.add(elementId);
                    }
                }
                initialFloors.add(floor);
            }
            for (int i = initialFloors.size(); i < 4; i++) {
                initialFloors.add(new HashSet<>());
            }

            int minSteps = solve(new State(0, initialFloors, 0), elementCount);
            System.out.println(minSteps);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static int solve(State initialState, int numPairs) {
        Queue<State> queue = new LinkedList<>();
        Set<State> visited = new HashSet<>();
        queue.offer(initialState);
        visited.add(initialState);

        while (!queue.isEmpty()) {
            State current = queue.poll();

            if (current.isComplete(numPairs)) {
                return current.steps;
            }

            List<Set<Integer>> combinations = getCombinations(current.floors.get(current.elevator));
            for (int direction : new int[]{-1, 1}) {
                int nextElevator = current.elevator + direction;
                if (nextElevator >= 0 && nextElevator < 4) {
                    for (Set<Integer> combination : combinations) {
                        List<Set<Integer>> nextFloors = new ArrayList<>();
                        for (int i = 0; i < 4; i++) {
                            nextFloors.add(new HashSet<>(current.floors.get(i)));
                        }
                        nextFloors.get(current.elevator).removeAll(combination);
                        nextFloors.get(nextElevator).addAll(combination);

                        State nextState = new State(nextElevator, nextFloors, current.steps + 1);
                        if (nextState.isSafe() && !visited.contains(nextState)) {
                            queue.offer(nextState);
                            visited.add(nextState);
                        }
                    }
                }
            }
        }

        return -1; // Should not reach here if a solution exists
    }

    private static List<Set<Integer>> getCombinations(Set<Integer> items) {
        List<Set<Integer>> combinations = new ArrayList<>();
        
        // Single items
        for (int item : items) {
            Set<Integer> single = new HashSet<>();
            single.add(item);
            combinations.add(single);
        }

        // Pairs of items
        List<Integer> itemList = new ArrayList<>(items);
        for (int i = 0; i < itemList.size(); i++) {
            for (int j = i + 1; j < itemList.size(); j++) {
                Set<Integer> pair = new HashSet<>();
                pair.add(itemList.get(i));
                pair.add(itemList.get(j));
                combinations.add(pair);
            }
        }
        
        return combinations;
    }
}
