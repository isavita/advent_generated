
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class MazeSolver {

    public static void main(String[] args) throws IOException {
        List<List<Character>> originalMap = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (!line.trim().isEmpty()) {
                    List<Character> row = new ArrayList<>();
                    for (char c : line.toCharArray()) {
                        row.add(c);
                    }
                    originalMap.add(row);
                }
            }
        }

        boolean found = false;
        for (int y = 1; y < originalMap.size() - 1; y++) {
            for (int x = 1; x < originalMap.get(0).size() - 1; x++) {
                if (originalMap.get(y).get(x) == '@') {
                    if (originalMap.get(y - 1).get(x) == '.' && originalMap.get(y + 1).get(x) == '.' &&
                            originalMap.get(y).get(x - 1) == '.' && originalMap.get(y).get(x + 1) == '.') {

                        originalMap.get(y - 1).set(x - 1, '@');
                        originalMap.get(y - 1).set(x, '#');
                        originalMap.get(y - 1).set(x + 1, '@');
                        originalMap.get(y).set(x - 1, '#');
                        originalMap.get(y).set(x, '#');
                        originalMap.get(y).set(x + 1, '#');
                        originalMap.get(y + 1).set(x - 1, '@');
                        originalMap.get(y + 1).set(x, '#');
                        originalMap.get(y + 1).set(x + 1, '@');
                        found = true;
                        break;
                    }
                }
            }
            if (found) {
                break;
            }
        }

        if (!found) {
            System.out.println("Error: Could not find the '@' symbol surrounded by open spaces.");
            System.exit(1);
        }
        List<int[]> robotPositions = new ArrayList<>();
        for (int y = 0; y < originalMap.size(); y++) {
            for (int x = 0; x < originalMap.get(y).size(); x++) {
                if (originalMap.get(y).get(x) == '@') {
                    robotPositions.add(new int[]{x, y});
                }
            }
        }

        Map<Character, int[]> keys = new HashMap<>();
        Map<Character, int[]> doors = new HashMap<>();
        Set<Character> allKeys = new HashSet<>();
        for (int y = 0; y < originalMap.size(); y++) {
            for (int x = 0; x < originalMap.get(y).size(); x++) {
                char cell = originalMap.get(y).get(x);
                if (Character.isLowerCase(cell)) {
                    keys.put(cell, new int[]{x, y});
                    allKeys.add(cell);
                } else if (Character.isUpperCase(cell)) {
                    doors.put(cell, new int[]{x, y});
                }
            }
        }

        Map<String, Map<Character, Pair>> keyGraph = new HashMap<>();
        List<String> allNodes = new ArrayList<>();
        allNodes.addAll(keys.keySet().stream().map(String::valueOf).toList());
        for (int i = 0; i < robotPositions.size(); i++) {
            allNodes.add("@" + i);
        }

        for (String key : allNodes)
             keyGraph.put(key, new HashMap<>());

        for(String fromKey : allNodes){
            int[] startPos;

            if (fromKey.startsWith("@")) {
              startPos = robotPositions.get(Integer.parseInt(fromKey.substring(1)));
            } else {
                startPos = keys.get(fromKey.charAt(0));
            }
            keyGraph.get(fromKey).putAll(bfs(startPos, originalMap,fromKey, keys));
        }


        System.out.println(dijkstra(keyGraph, robotPositions, allKeys));
    }

    static Map<Character, Pair> bfs(int[] startPos, List<List<Character>> map, String startKey, Map<Character, int[]> keys) {
        Deque<State> queue = new ArrayDeque<>();
        queue.add(new State(startPos[0], startPos[1], 0, new HashSet<>()));
        Set<PairXY> visited = new HashSet<>();
        Map<Character, Pair> results = new HashMap<>();

        while (!queue.isEmpty()) {
            State current = queue.poll();
            int x = current.x;
            int y = current.y;
            int dist = current.dist;
            Set<Character> requiredKeys = current.requiredKeys;

             PairXY p = new PairXY(x,y);

            if (visited.contains(p)) {
                continue;
            }
            visited.add(p);

            char cell = map.get(y).get(x);

            if (Character.isLowerCase(cell) &&  (startKey.length() ==1? cell != startKey.charAt(0) : true)  && !requiredKeys.contains(cell)) {
                results.put(cell, new Pair(dist, new HashSet<>(requiredKeys)));
                requiredKeys = new HashSet<>(requiredKeys);
                requiredKeys.add(cell);
            }

            int[][] directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};
            for (int[] dir : directions) {
                int nx = x + dir[0];
                int ny = y + dir[1];

                if (ny >= 0 && ny < map.size() && nx >= 0 && nx < map.get(0).size()) {
                    char ncell = map.get(ny).get(nx);
                    if (ncell != '#') {
                        Set<Character> nRequiredKeys = new HashSet<>(requiredKeys);
                        if (Character.isUpperCase(ncell)) {
                            nRequiredKeys.add(Character.toLowerCase(ncell));
                            queue.add(new State(nx, ny, dist + 1, nRequiredKeys));
                        } else {
                            queue.add(new State(nx, ny, dist + 1, nRequiredKeys));
                        }
                    }
                }
            }
        }
        return results;
    }



    static int dijkstra(Map<String, Map<Character, Pair>> keyGraph, List<int[]> robotPositions, Set<Character> allKeys) {
        int totalKeys = allKeys.size();
        List<String> initialPositions = new ArrayList<>();
        for (int i = 0; i < robotPositions.size(); i++) {
            initialPositions.add("@" + i);
        }

        PriorityQueue<RobotState> heap = new PriorityQueue<>(Comparator.comparingInt(a -> a.cost));
        heap.add(new RobotState(0, initialPositions, new HashSet<>()));

        Map<Key, Integer> visited = new HashMap<>();

        while (!heap.isEmpty()) {
            RobotState current = heap.poll();
            int cost = current.cost;
            List<String> positions = current.positions;
            Set<Character> collectedKeys = current.collectedKeys;

            Key state = new Key(positions, bitmask(collectedKeys));

            if (visited.containsKey(state) && visited.get(state) <= cost) {
                continue;
            }
            visited.put(state, cost);

            if (collectedKeys.size() == totalKeys) {
                return cost;
            }

            for (int i = 0; i < positions.size(); i++) {
                String pos = positions.get(i);

                for(Map.Entry<Character, Pair> entry : keyGraph.get(pos).entrySet()){

                    Character key = entry.getKey();
                    Pair pair  = entry.getValue();

                    int dist = pair.dist;
                    Set<Character> required_keys = pair.requiredKeys;

                    if (!collectedKeys.contains(key) && required_keys.stream().allMatch(collectedKeys::contains)) {

                            List<String> newPositions = new ArrayList<>(positions);
                            newPositions.set(i, String.valueOf(key));
                            Set<Character> newCollectedKeys = new HashSet<>(collectedKeys);
                            newCollectedKeys.add(key);

                           Key newState = new Key(newPositions, bitmask(newCollectedKeys));

                            if (visited.containsKey(newState) && visited.get(newState) <= cost+dist)
                               continue;


                            heap.add(new RobotState(cost + dist, newPositions, newCollectedKeys));
                    }

                }

            }
        }

        return -1; // Should not reach here if all keys are reachable
    }

    static int bitmask(Set<Character> keysSet) {
        int mask = 0;
        for (char k : keysSet) {
            mask |= 1 << (k - 'a');
        }
        return mask;
    }

    static class Pair {
        int dist;
        Set<Character> requiredKeys;

        Pair(int dist, Set<Character> requiredKeys) {
            this.dist = dist;
            this.requiredKeys = requiredKeys;
        }
    }

    static class State {
        int x, y, dist;
        Set<Character> requiredKeys;

        State(int x, int y, int dist, Set<Character> requiredKeys) {
            this.x = x;
            this.y = y;
            this.dist = dist;
            this.requiredKeys = requiredKeys;
        }
    }

    static class RobotState {
        int cost;
        List<String> positions;
        Set<Character> collectedKeys;

        RobotState(int cost, List<String> positions, Set<Character> collectedKeys) {
            this.cost = cost;
            this.positions = positions;
            this.collectedKeys = collectedKeys;
        }
    }
      static class PairXY{
        int x;
        int y;
        public PairXY(int x, int y){
            this.x = x;
            this.y = y;
        }
          @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            PairXY that = (PairXY) o;
            return x == that.x && y == that.y;
        }

        @Override
        public int hashCode() {
            return Objects.hash(x, y);
        }
    }

    static class Key {
        List<String> positions;
        int collectedKeysMask;

        Key(List<String> positions, int collectedKeysMask) {
            this.positions = positions;
            this.collectedKeysMask = collectedKeysMask;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Key key = (Key) o;
            return collectedKeysMask == key.collectedKeysMask &&
                    Objects.equals(positions, key.positions);
        }

        @Override
        public int hashCode() {
            return Objects.hash(positions, collectedKeysMask);
        }
    }
}
