
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class BeverageBandits {

    private static final int ELF_START_HP = 200;
    private static final int GOBLIN_ATTACK_POWER = 3;

    public static void main(String[] args) {
        try {
            List<String> lines = readInput("input.txt");
            char[][] grid = parseGrid(lines);

            // Part 1
            int outcome1 = simulateCombat(cloneGrid(grid), 3, false);
            System.out.println("Part 1 Outcome: " + outcome1);

            // Part 2
            int elfAttackPower = 4;
            int outcome2 = 0;
            while (true) {
                outcome2 = simulateCombat(cloneGrid(grid), elfAttackPower, true);
                if (outcome2 != -1) {
                    break;
                }
                elfAttackPower++;
            }
            System.out.println("Part 2 Outcome: " + outcome2 + " (Elf Attack Power: " + elfAttackPower + ")");


        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static List<String> readInput(String filename) throws IOException {
        List<String> lines = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
        }
        return lines;
    }

    private static char[][] parseGrid(List<String> lines) {
        int rows = lines.size();
        int cols = lines.get(0).length();
        char[][] grid = new char[rows][cols];
        for (int i = 0; i < rows; i++) {
            grid[i] = lines.get(i).toCharArray();
        }
        return grid;
    }

    private static char[][] cloneGrid(char[][] grid) {
        char[][] clonedGrid = new char[grid.length][grid[0].length];
        for (int i = 0; i < grid.length; i++) {
            clonedGrid[i] = Arrays.copyOf(grid[i], grid[i].length);
        }
        return clonedGrid;
    }


    private static int simulateCombat(char[][] grid, int elfAttackPower, boolean noElfDeaths) {
        int rows = grid.length;
        int cols = grid[0].length;
        int fullRounds = 0;
        int initialElfCount = 0;
        for(int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if(grid[r][c] == 'E') initialElfCount++;
            }
        }


        Map<Point, Integer> hitPoints = new HashMap<>();
        initializeHitPoints(grid, hitPoints);

        while (true) {
            List<Point> units = getUnits(grid);
            boolean combatEndedThisRound = false;

            for (Point unit : units) {
                if (!hitPoints.containsKey(unit)) continue; // Unit died earlier in round

                if (getEnemies(grid, unit).isEmpty()) {
                    combatEndedThisRound = true;
                    break; // Combat ends
                }

                // Move
                if (!isInRange(grid, unit)) {
                    Point moveTarget = chooseMove(grid, unit, hitPoints);
                    if (moveTarget != null) {
                        char unitType = grid[unit.row][unit.col];
                        grid[unit.row][unit.col] = '.';
                        grid[moveTarget.row][moveTarget.col] = unitType;
                        hitPoints.put(moveTarget, hitPoints.remove(unit));
                        unit.row = moveTarget.row; //Important for correct reading order
                        unit.col = moveTarget.col;
                    }
                }


                // Attack
                Point attackTarget = chooseAttackTarget(grid, unit, hitPoints);
                if (attackTarget != null) {
                    int attackPower = (grid[unit.row][unit.col] == 'E') ? elfAttackPower : GOBLIN_ATTACK_POWER;
                    hitPoints.put(attackTarget, hitPoints.get(attackTarget) - attackPower);

                    if (hitPoints.get(attackTarget) <= 0) {
                        if (noElfDeaths && grid[attackTarget.row][attackTarget.col] == 'E') return -1; //elf died
                        grid[attackTarget.row][attackTarget.col] = '.';
                        hitPoints.remove(attackTarget);
                    }
                }

            }

            if (combatEndedThisRound) break;
            fullRounds++;

        }

        int totalHitPoints = 0;
        for (int hp : hitPoints.values()) {
            totalHitPoints += hp;
        }

        return fullRounds * totalHitPoints;
    }



    private static void initializeHitPoints(char[][] grid, Map<Point, Integer> hitPoints) {
        for (int r = 0; r < grid.length; r++) {
            for (int c = 0; c < grid[0].length; c++) {
                if (grid[r][c] == 'G' || grid[r][c] == 'E') {
                    hitPoints.put(new Point(r, c), ELF_START_HP);
                }
            }
        }
    }

    private static List<Point> getUnits(char[][] grid) {
        List<Point> units = new ArrayList<>();
        for (int r = 0; r < grid.length; r++) {
            for (int c = 0; c < grid[0].length; c++) {
                if (grid[r][c] == 'G' || grid[r][c] == 'E') {
                    units.add(new Point(r, c));
                }
            }
        }
        return units;
    }

    private static List<Point> getEnemies(char[][] grid, Point unit) {
        List<Point> enemies = new ArrayList<>();
        char unitType = grid[unit.row][unit.col];
        char enemyType = (unitType == 'G') ? 'E' : 'G';
        for (int r = 0; r < grid.length; r++) {
            for (int c = 0; c < grid[0].length; c++) {
                if (grid[r][c] == enemyType) {
                    enemies.add(new Point(r, c));
                }
            }
        }
        return enemies;
    }

    private static boolean isInRange(char[][] grid, Point unit) {
        char unitType = grid[unit.row][unit.col];
        char enemyType = (unitType == 'G') ? 'E' : 'G';
        int[][] directions = {{-1, 0}, {0, -1}, {0, 1}, {1, 0}};

        for (int[] dir : directions) {
            int nr = unit.row + dir[0];
            int nc = unit.col + dir[1];
            if (nr >= 0 && nr < grid.length && nc >= 0 && nc < grid[0].length && grid[nr][nc] == enemyType) {
                return true;
            }
        }
        return false;
    }
    private static Point chooseMove(char[][] grid, Point unit, Map<Point, Integer> hitPoints) {
        char unitType = grid[unit.row][unit.col];
        char enemyType = (unitType == 'G') ? 'E' : 'G';
        int[][] directions = {{-1, 0}, {0, -1}, {0, 1}, {1, 0}};

        // Find in-range squares
        Set<Point> inRangeSquares = new HashSet<>();
        for (int r = 0; r < grid.length; r++) {
            for (int c = 0; c < grid[0].length; c++) {
                if (grid[r][c] == enemyType) {
                    for (int[] dir : directions) {
                        int nr = r + dir[0];
                        int nc = c + dir[1];
                        if (nr >= 0 && nr < grid.length && nc >= 0 && nc < grid[0].length && grid[nr][nc] == '.') {
                            inRangeSquares.add(new Point(nr, nc));
                        }
                    }
                }
            }
        }

        if (inRangeSquares.isEmpty()) return null;


        // BFS to find reachable squares and their distances
        Map<Point, Integer> distances = new HashMap<>();
        Queue<Point> queue = new LinkedList<>();
        queue.add(unit);
        distances.put(unit, 0);
        Map<Point, Point> cameFrom = new HashMap<>(); //For path reconstruction.

        while (!queue.isEmpty()) {
            Point current = queue.poll();

            for (int[] dir : directions) {
                int nr = current.row + dir[0];
                int nc = current.col + dir[1];
                Point next = new Point(nr, nc);

                if (nr >= 0 && nr < grid.length && nc >= 0 && nc < grid[0].length &&
                        (grid[nr][nc] == '.' || (nr == unit.row && nc == unit.col))  && !distances.containsKey(next))
                {
                    distances.put(next, distances.get(current) + 1);
                    cameFrom.put(next, current);
                    queue.add(next);
                }
            }
        }

        // Filter reachable in-range squares
        List<Point> reachableInRange = new ArrayList<>();
        for (Point p : inRangeSquares) {
            if (distances.containsKey(p)) {
                reachableInRange.add(p);
            }
        }

        if (reachableInRange.isEmpty()) return null;

        // Find nearest reachable in-range squares
        int minDistance = Integer.MAX_VALUE;
        for (Point p : reachableInRange) {
            minDistance = Math.min(minDistance, distances.get(p));
        }

        List<Point> nearestSquares = new ArrayList<>();
        for (Point p : reachableInRange) {
            if (distances.get(p) == minDistance) {
                nearestSquares.add(p);
            }
        }

        // Choose target by reading order
        Collections.sort(nearestSquares);
        Point chosenTarget = nearestSquares.get(0);

        //Reconstruct path and select first step.
        List<Point> path = new ArrayList<>();
        Point current = chosenTarget;
        while(!current.equals(unit)) {
            path.add(current);
            current = cameFrom.get(current);
        }
        Collections.reverse(path);

        if(path.isEmpty()) return null; //Should not happen, as we already checked reachability

        return path.get(0); // Return first step.
    }



    private static Point chooseAttackTarget(char[][] grid, Point unit, Map<Point, Integer> hitPoints) {
        char unitType = grid[unit.row][unit.col];
        char enemyType = (unitType == 'G') ? 'E' : 'G';
        int[][] directions = {{-1, 0}, {0, -1}, {0, 1}, {1, 0}};
        List<Point> adjacentEnemies = new ArrayList<>();

        for (int[] dir : directions) {
            int nr = unit.row + dir[0];
            int nc = unit.col + dir[1];
            if (nr >= 0 && nr < grid.length && nc >= 0 && nc < grid[0].length && grid[nr][nc] == enemyType) {
                adjacentEnemies.add(new Point(nr, nc));
            }
        }

        if (adjacentEnemies.isEmpty()) return null;

        Point target = null;
        int minHp = Integer.MAX_VALUE;
        for(Point enemy : adjacentEnemies) {
            int hp = hitPoints.get(enemy);
            if (hp < minHp) {
                minHp = hp;
                target = enemy;
            } else if (hp == minHp && (enemy.row < target.row || (enemy.row == target.row && enemy.col < target.col)))
            {
                target = enemy;
            }
        }

        return target;
    }


    static class Point implements Comparable<Point> {
        int row;
        int col;

        public Point(int row, int col) {
            this.row = row;
            this.col = col;
        }

        @Override
        public int compareTo(Point other) {
            if (this.row != other.row) {
                return this.row - other.row;
            }
            return this.col - other.col;
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

        @Override
        public String toString(){
            return "(" + row + ", " + col + ")";
        }
    }
}
