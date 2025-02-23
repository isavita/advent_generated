
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Amphipod {

    static class State implements Comparable<State> {
        char[][] grid;
        int energyUsed;
        String path;

        public State(char[][] grid, int energyUsed, String path) {
            this.grid = grid;
            this.energyUsed = energyUsed;
            this.path = path;
        }

        @Override
        public int compareTo(State other) {
            return Integer.compare(this.energyUsed, other.energyUsed);
        }

        public State copy() {
            char[][] newGrid = new char[grid.length][];
            for (int i = 0; i < grid.length; i++) {
                newGrid[i] = Arrays.copyOf(grid[i], grid[i].length);
            }
            return new State(newGrid, energyUsed, path);
        }

        public boolean isAllDone(Map<Point, Character> roomCoordToWantChar) {
            for (Map.Entry<Point, Character> entry : roomCoordToWantChar.entrySet()) {
                Point coord = entry.getKey();
                char want = entry.getValue();
                if (grid[coord.row][coord.col] != want) {
                    return false;
                }
            }
            return true;
        }

        public List<Point> getUnsettledCoords(Map<Point, Character> roomCoordToWantChar) {
            List<Point> unsettled = new ArrayList<>();
            for (int col = 1; col < grid[0].length; col++) {
                if ("ABCD".indexOf(grid[1][col]) != -1) {
                    unsettled.add(new Point(1, col));
                }
            }

            for (int col : new int[]{3, 5, 7, 9}) {
                boolean roomFullFromBack = true;
                for (int row = grid.length - 2; row > 1; row--) {
                    Point coord = new Point(row, col);
                    Character wantChar = roomCoordToWantChar.get(coord);
                    char gotChar = grid[row][col];
                    if (gotChar != '.') {
                        if (gotChar != wantChar) {
                            roomFullFromBack = false;
                            unsettled.add(coord);
                        } else if (gotChar == wantChar && !roomFullFromBack) {
                            unsettled.add(coord);
                        }
                    }
                }
            }
            return unsettled;
        }

        public List<Point> getNextPossibleMoves(Point unsettledCoord, Map<Point, Character> roomCoordToWantChar) {
            char unsettledChar = grid[unsettledCoord.row][unsettledCoord.col];
            if ("ABCD".indexOf(unsettledChar) == -1) {
                throw new IllegalArgumentException("Unexpected character to get next moves for: " + unsettledChar);
            }

            List<Point> possible = new ArrayList<>();
            boolean startedInHallway = unsettledCoord.row == 1;

            Queue<Point> queue = new LinkedList<>();
            queue.add(unsettledCoord);
            Set<Point> seen = new HashSet<>();

            while (!queue.isEmpty()) {
                Point front = queue.poll();
                if (seen.contains(front)) {
                    continue;
                }
                seen.add(front);

                if (!front.equals(unsettledCoord)) {
                    if (!(front.row == 1 && (front.col == 3 || front.col == 5 || front.col == 7 || front.col == 9))) {
                        Character wantChar = roomCoordToWantChar.get(front);
                        if (wantChar == null) {
                            if (!startedInHallway) {
                                possible.add(front);
                            }
                        } else if (wantChar == unsettledChar) {
                            boolean isStuckAmphipod = false;
                            boolean roomHasDeeperOpenSpaces = false;
                            for (int r = front.row + 1; r < grid.length - 1; r++) {
                                char c = grid[r][front.col];
                                if (c == '.') {
                                    roomHasDeeperOpenSpaces = true;
                                } else if (c != '.' && c != unsettledChar) {
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
                for (int[] dir : directions) {
                    int nextRow = front.row + dir[0];
                    int nextCol = front.col + dir[1];
                    if (nextRow >= 0 && nextRow < grid.length && nextCol >= 0 && nextCol < grid[0].length && grid[nextRow][nextCol] == '.') {
                        queue.add(new Point(nextRow, nextCol));
                    }
                }
            }
            return possible;
        }
    }

    static class Point {
        int row, col;

        public Point(int row, int col) {
            this.row = row;
            this.col = col;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Point point = (Point) o;
            return row == point.row && col == point.col;
        }

        @Override
        public int hashCode() {
            return Objects.hash(row, col);
        }
    }

    static int calcEnergy(char c, Point start, Point end) {
        int dist = Math.abs(end.col - start.col);
        dist += start.row - 1;
        dist += end.row - 1;
        int energyPerType = switch (c) {
            case 'A' -> 1;
            case 'B' -> 10;
            case 'C' -> 100;
            case 'D' -> 1000;
            default -> throw new IllegalArgumentException("Unexpected character: " + c);
        };
        return energyPerType * dist;
    }

    public static int amphipod(String inputStr) {
        State start = new State(
                Arrays.stream(inputStr.split("\n")).map(String::toCharArray).toArray(char[][]::new),
                0,
                ""
        );

        Map<Point, Character> roomCoordToWantChar = new HashMap<>();
        roomCoordToWantChar.put(new Point(2, 3), 'A');
        roomCoordToWantChar.put(new Point(3, 3), 'A');
        roomCoordToWantChar.put(new Point(4, 3), 'A');
        roomCoordToWantChar.put(new Point(5, 3), 'A');
        roomCoordToWantChar.put(new Point(2, 5), 'B');
        roomCoordToWantChar.put(new Point(3, 5), 'B');
        roomCoordToWantChar.put(new Point(4, 5), 'B');
        roomCoordToWantChar.put(new Point(5, 5), 'B');
        roomCoordToWantChar.put(new Point(2, 7), 'C');
        roomCoordToWantChar.put(new Point(3, 7), 'C');
        roomCoordToWantChar.put(new Point(4, 7), 'C');
        roomCoordToWantChar.put(new Point(5, 7), 'C');
        roomCoordToWantChar.put(new Point(2, 9), 'D');
        roomCoordToWantChar.put(new Point(3, 9), 'D');
        roomCoordToWantChar.put(new Point(4, 9), 'D');
        roomCoordToWantChar.put(new Point(5, 9), 'D');

        start.grid = Arrays.copyOf(start.grid, 7);
        start.grid[6] = start.grid[4];
        start.grid[5] = start.grid[3];
        start.grid[3] = "  #D#C#B#A#  ".toCharArray();
        start.grid[4] = "  #D#B#A#C#  ".toCharArray();


        PriorityQueue<State> minHeap = new PriorityQueue<>();
        minHeap.add(start);
        Set<String> seenGrids = new HashSet<>();

        while (!minHeap.isEmpty()) {
            State front = minHeap.poll();
            String key = Arrays.deepToString(front.grid);
            if (seenGrids.contains(key)) {
                continue;
            }
            seenGrids.add(key);

            if (front.isAllDone(roomCoordToWantChar)) {
                return front.energyUsed;
            }

            List<Point> unsettledCoords = front.getUnsettledCoords(roomCoordToWantChar);
            for (Point unsettledCoord : unsettledCoords) {
                List<Point> nextMoves = front.getNextPossibleMoves(unsettledCoord, roomCoordToWantChar);
                for (Point nextCoord : nextMoves) {
                    if (front.grid[nextCoord.row][nextCoord.col] != '.') {
                        throw new IllegalArgumentException("Should only be moving to walkable spaces, got " + front.grid[nextCoord.row][nextCoord.col] + " at " + nextCoord);
                    }

                    State cp = front.copy();
                    cp.energyUsed += calcEnergy(cp.grid[unsettledCoord.row][unsettledCoord.col], unsettledCoord, nextCoord);
                    cp.path += String.format("%c%s->%s(%d),", cp.grid[unsettledCoord.row][unsettledCoord.col], unsettledCoord, nextCoord, cp.energyUsed);
                    cp.grid[nextCoord.row][nextCoord.col] = cp.grid[unsettledCoord.row][unsettledCoord.col];
                    cp.grid[unsettledCoord.row][unsettledCoord.col] = '.';
                    minHeap.add(cp);
                }
            }
        }
        throw new IllegalArgumentException("Should return from loop");
    }

    public static void main(String[] args) throws IOException {
        String inputStr = new String(Files.readAllBytes(Paths.get("input.txt"))).trim();
        System.out.println(amphipod(inputStr));
    }
}
