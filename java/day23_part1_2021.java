
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

class Solution {

    private static final Map<List<Integer>, String> roomCoordToWantChar = Map.of(
            List.of(2, 3), "A", List.of(3, 3), "A",
            List.of(2, 5), "B", List.of(3, 5), "B",
            List.of(2, 7), "C", List.of(3, 7), "C",
            List.of(2, 9), "D", List.of(3, 9), "D"
    );

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            StringBuilder sb = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                sb.append(line).append("\n");
            }
            reader.close();
            String input = sb.toString().trim();
            System.out.println(amphipod(input));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static int amphipod(String input) {
        State start = parseInput(input);
        PriorityQueue<State> minHeap = new PriorityQueue<>(Comparator.comparingInt(s -> s.energyUsed));
        minHeap.add(start);
        Set<String> seenGrids = new HashSet<>();

        while (!minHeap.isEmpty()) {
            State front = minHeap.poll();
            String key = front.gridToString();
            if (seenGrids.contains(key)) {
                continue;
            }
            seenGrids.add(key);

            if (front.allDone(roomCoordToWantChar)) {
                return front.energyUsed;
            }

            List<List<Integer>> unsettledCoords = front.getUnsettledCoords(roomCoordToWantChar);
            for (List<Integer> unsettledCoord : unsettledCoords) {
                int ur = unsettledCoord.get(0);
                int uc = unsettledCoord.get(1);
                List<List<Integer>> nextMoves = front.getNextPossibleMoves(unsettledCoord, roomCoordToWantChar);
                for (List<Integer> nextCoord : nextMoves) {
                    int nr = nextCoord.get(0);
                    int nc = nextCoord.get(1);
                    if (!front.grid[nr][nc].equals(".")) {
                        throw new IllegalStateException("should only be moving to walkable spaces, got " + front.grid[nr][nc] + " at " + nr + "," + nc);
                    }

                    State cp = front.copy();
                    cp.energyUsed += calcEnergy(cp.grid[ur][uc], unsettledCoord, nextCoord);
                    cp.path += String.format("%s%s->%s{%d},", cp.grid[ur][uc], unsettledCoord, nextCoord, cp.energyUsed);
                    String temp = cp.grid[nr][nc];
                    cp.grid[nr][nc] = cp.grid[ur][uc];
                    cp.grid[ur][uc] = temp;

                    minHeap.add(cp);
                }
            }
        }
        throw new IllegalStateException("should return from loop");
    }

    private static State parseInput(String input) {
        String[] lines = input.split("\n");
        String[][] grid = new String[lines.length][];
        for (int i = 0; i < lines.length; i++) {
            grid[i] = lines[i].split("");
        }
        return new State(grid);
    }

    static class State {
        String[][] grid;
        int energyUsed;
        String path;

        State(String[][] grid) {
            this.grid = grid;
            this.energyUsed = 0;
            this.path = "";
        }

        public String gridToString() {
            StringBuilder sb = new StringBuilder();
            for (String[] row : grid) {
                for (String s : row) {
                    sb.append(s);
                }
                sb.append("\n");
            }
            return sb.toString();
        }

        public State copy() {
            String[][] newGrid = new String[grid.length][];
            for (int i = 0; i < grid.length; i++) {
                newGrid[i] = Arrays.copyOf(grid[i], grid[i].length);
            }
            State copy = new State(newGrid);
            copy.energyUsed = this.energyUsed;
            copy.path = this.path;
            return copy;
        }

        public boolean allDone(Map<List<Integer>, String> roomCoordToWantChar) {
            for (Map.Entry<List<Integer>, String> entry : roomCoordToWantChar.entrySet()) {
                List<Integer> coord = entry.getKey();
                String want = entry.getValue();
                if (!grid[coord.get(0)][coord.get(1)].equals(want)) {
                    return false;
                }
            }
            return true;
        }

        public List<List<Integer>> getUnsettledCoords(Map<List<Integer>, String> roomCoordToWantChar) {
            List<List<Integer>> unsettled = new ArrayList<>();

            for (int col = 1; col < grid[0].length; col++) {
                if ("ABCD".contains(grid[1][col])) {
                    unsettled.add(List.of(1, col));
                }
            }

            for (int col : new int[]{3, 5, 7, 9}) {
                boolean roomFullFromBack = true;
                for (int row = grid.length - 2; row >= 2; row--) {
                    List<Integer> coord = List.of(row, col);
                    String wantChar = roomCoordToWantChar.get(coord);
                    String gotChar = grid[row][col];
                    if (!gotChar.equals(".")) {
                        if (!gotChar.equals(wantChar)) {
                            roomFullFromBack = false;
                            unsettled.add(coord);
                        } else if (gotChar.equals(wantChar) && !roomFullFromBack) {
                            unsettled.add(coord);
                        }
                    }
                }
            }
            return unsettled;
        }

        private static final Map<List<Integer>, Boolean> coordsInFrontOfRooms = Map.of(
                List.of(1, 3), true,
                List.of(1, 5), true,
                List.of(1, 7), true,
                List.of(1, 9), true
        );

        private boolean isInHallway(List<Integer> coord) {
            return coord.get(0) == 1;
        }

        public List<List<Integer>> getNextPossibleMoves(List<Integer> unsettledCoord, Map<List<Integer>, String> roomCoordToWantChar) {
            String unsettledChar = grid[unsettledCoord.get(0)][unsettledCoord.get(1)];
            if (!"ABCD".contains(unsettledChar)) {
                throw new IllegalArgumentException("unexpected character to get next moves for " + unsettledChar);
            }

            List<List<Integer>> possible = new ArrayList<>();
            boolean startedInHallway = isInHallway(unsettledCoord);

            Queue<List<Integer>> queue = new LinkedList<>();
            queue.add(unsettledCoord);
            Set<List<Integer>> seen = new HashSet<>();

            while (!queue.isEmpty()) {
                List<Integer> front = queue.poll();
                if (seen.contains(front)) {
                    continue;
                }
                seen.add(front);

                if (!front.equals(unsettledCoord)) {
                    if (!coordsInFrontOfRooms.containsKey(front)) {
                        String wantChar = roomCoordToWantChar.get(front);
                        if (wantChar == null) {
                            if (!startedInHallway) {
                                possible.add(front);
                            }
                        } else if (wantChar.equals(unsettledChar)) {
                            boolean isStuckAmphipod = false;
                            boolean roomHasDeeperOpenSpaces = false;
                            for (int r = front.get(0) + 1; r < grid.length - 1; r++) {
                                String charAtR = grid[r][front.get(1)];
                                if (charAtR.equals(".")) {
                                    roomHasDeeperOpenSpaces = true;
                                }
                                if (!charAtR.equals(".") && !charAtR.equals(unsettledChar)) {
                                    isStuckAmphipod = true;
                                    break;
                                }
                            }
                            if (!roomHasDeeperOpenSpaces && !isStuckAmphipod) {
                                possible.add(front);
                            }
                        }
                    }
                }

                int[][] directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};
                for (int[] d : directions) {
                    List<Integer> next = List.of(front.get(0) + d[0], front.get(1) + d[1]);
                    if (grid[next.get(0)][next.get(1)].equals(".")) {
                        queue.add(next);
                    }
                }
            }
            return possible;
        }
    }

    private static int calcEnergy(String character, List<Integer> start, List<Integer> end) {
        int dist = Math.abs(end.get(1) - start.get(1));
        dist += start.get(0) - 1;
        dist += end.get(0) - 1;

        Map<String, Integer> energyPerType = Map.of(
                "A", 1,
                "B", 10,
                "C", 100,
                "D", 1000
        );

        if (!energyPerType.containsKey(character)) {
            throw new IllegalArgumentException(character + " should not call calcEnergy()");
        }
        return energyPerType.get(character) * dist;
    }
}
