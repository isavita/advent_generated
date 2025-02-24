
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class RadioisotopeThermoelectricGenerators {

    private static final int MAX_FLOORS = 4;

    public static void main(String[] args) {
        String inputFile = "input.txt";
        List<Set<String>> initialFloors = readInput(inputFile);

        int minSteps = solve(initialFloors, false);
        System.out.println("Minimum steps (Part 1): " + minSteps);

        // Part 2: Add elerium and dilithium items to the first floor
        initialFloors.get(0).add("EG"); // Elerium Generator
        initialFloors.get(0).add("EM"); // Elerium Microchip
        initialFloors.get(0).add("DG"); // Dilithium Generator
        initialFloors.get(0).add("DM"); // Dilithium Microchip

        int minStepsPart2 = solve(initialFloors, true);
        System.out.println("Minimum steps (Part 2): " + minStepsPart2);
    }

    private static List<Set<String>> readInput(String inputFile) {
        List<Set<String>> floors = new ArrayList<>();
        for (int i = 0; i < MAX_FLOORS; i++) {
            floors.add(new HashSet<>());
        }

        try (BufferedReader br = new BufferedReader(new FileReader(inputFile))) {
            String line;
            int floorNum = 0;
            while ((line = br.readLine()) != null) {
                //Simplified parsing based on keywords, handles variations.
                String[] parts = line.split("a |an |and |, |\\.");
                for (String part : parts) {
                     if (part.contains("generator")) {
                        String element = part.substring(0,part.indexOf("-")).toUpperCase().substring(0,1)+"G";
                        floors.get(floorNum).add(element);
                    } else if (part.contains("microchip")) {
                        String element = part.substring(0,part.indexOf("-")).toUpperCase().substring(0,1) + "M";
                         floors.get(floorNum).add(element);
                    }

                }
               floorNum++;
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return floors;
    }


    private static int solve(List<Set<String>> initialFloors, boolean part2) {
        State initialState = new State(0, initialFloors);
        Queue<State> queue = new LinkedList<>();
        queue.offer(initialState);
        Set<String> visited = new HashSet<>();
        visited.add(initialState.getHash());

        int steps = 0;
        while (!queue.isEmpty()) {
            int size = queue.size();
            for (int i = 0; i < size; i++) {
                State current = queue.poll();

                if (isGoal(current)) {
                    return steps;
                }

                List<State> nextStates = generateNextStates(current);
                for (State nextState : nextStates) {
                    String hash = nextState.getHash();
                    if (!visited.contains(hash) && isValid(nextState)) {
                        visited.add(hash);
                        queue.offer(nextState);
                    }
                }
            }
            steps++;
        }

        return -1; // Should not reach here if a solution exists
    }

    private static boolean isGoal(State state) {
        for (int i = 0; i < MAX_FLOORS - 1; i++) {
            if (!state.floors.get(i).isEmpty()) {
                return false;
            }
        }
        return true;
    }

    private static List<State> generateNextStates(State state) {
        List<State> nextStates = new ArrayList<>();
        int currentFloor = state.elevatorFloor;
        Set<String> currentItems = state.floors.get(currentFloor);

        // Generate combinations of 1 or 2 items to move
        List<List<String>> combinations = new ArrayList<>();
        List<String> currentItemList = new ArrayList<>(currentItems);

        for (int i = 0; i < currentItemList.size(); i++) {
            combinations.add(List.of(currentItemList.get(i)));
            for (int j = i + 1; j < currentItemList.size(); j++) {
                combinations.add(List.of(currentItemList.get(i), currentItemList.get(j)));
            }
        }


        // Move up
        if (currentFloor < MAX_FLOORS - 1) {
            for (List<String> itemsToMove : combinations) {
                State nextState = new State(currentFloor + 1, deepCopyFloors(state.floors));
                nextState.floors.get(currentFloor).removeAll(itemsToMove);
                nextState.floors.get(currentFloor + 1).addAll(itemsToMove);
                nextStates.add(nextState);
            }
        }

        // Move down
        if (currentFloor > 0) {
            for (List<String> itemsToMove : combinations) {
                 State nextState = new State(currentFloor - 1, deepCopyFloors(state.floors));
                nextState.floors.get(currentFloor).removeAll(itemsToMove);
                nextState.floors.get(currentFloor - 1).addAll(itemsToMove);
                nextStates.add(nextState);
            }
        }

        return nextStates;
    }

    private static boolean isValid(State state) {
        for (Set<String> floor : state.floors) {
            if (!isFloorValid(floor)) {
                return false;
            }
        }
        return true;
    }

    private static boolean isFloorValid(Set<String> floor) {
        Set<String> generators = new HashSet<>();
        Set<String> microchips = new HashSet<>();

        for (String item : floor) {
            if (item.endsWith("G")) {
                generators.add(item.substring(0, item.length() - 1));
            } else {
                microchips.add(item.substring(0, item.length() - 1));
            }
        }
      
        for (String microchip : microchips) {
             if (!generators.contains(microchip) && !generators.isEmpty() )  return false;

        }

        return true;
    }


    private static List<Set<String>> deepCopyFloors(List<Set<String>> floors) {
        List<Set<String>> copy = new ArrayList<>();
        for (Set<String> floor : floors) {
            copy.add(new HashSet<>(floor));
        }
        return copy;
    }

    private static class State {
        int elevatorFloor;
        List<Set<String>> floors;

        State(int elevatorFloor, List<Set<String>> floors) {
            this.elevatorFloor = elevatorFloor;
            this.floors = floors;
        }

        String getHash() {
            StringBuilder sb = new StringBuilder();
            sb.append(elevatorFloor);
            for (int i = 0; i < MAX_FLOORS; i++) {
                List<String> sortedItems = new ArrayList<>(floors.get(i));
                Collections.sort(sortedItems);
                sb.append(i);
                for (String item : sortedItems) {
                    sb.append(item);
                }
            }
            return sb.toString();
        }
    }
}
