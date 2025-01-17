
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class CosmicExpansion {

    public static void main(String[] args) {
        try {
            List<String> lines = readInput("input.txt");
            char[][] grid = parseGrid(lines);

            System.out.println("Part 1: " + solve(grid, 2));
            System.out.println("Part 2: " + solve(grid, 1000000));

        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static long solve(char[][] grid, int expansionFactor) {
        List<Integer> emptyRows = findEmptyRows(grid);
        List<Integer> emptyCols = findEmptyCols(grid);
        List<Point> galaxies = findGalaxies(grid);

        long totalDistance = 0;
        for (int i = 0; i < galaxies.size(); i++) {
            for (int j = i + 1; j < galaxies.size(); j++) {
                totalDistance += calculateDistance(galaxies.get(i), galaxies.get(j), emptyRows, emptyCols, expansionFactor);
            }
        }
        return totalDistance;
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

    private static List<Integer> findEmptyRows(char[][] grid) {
        List<Integer> emptyRows = new ArrayList<>();
        for (int i = 0; i < grid.length; i++) {
            boolean isEmpty = true;
            for (int j = 0; j < grid[0].length; j++) {
                if (grid[i][j] == '#') {
                    isEmpty = false;
                    break;
                }
            }
            if (isEmpty) {
                emptyRows.add(i);
            }
        }
        return emptyRows;
    }

    private static List<Integer> findEmptyCols(char[][] grid) {
        List<Integer> emptyCols = new ArrayList<>();
        for (int j = 0; j < grid[0].length; j++) {
            boolean isEmpty = true;
            for (int i = 0; i < grid.length; i++) {
                if (grid[i][j] == '#') {
                    isEmpty = false;
                    break;
                }
            }
            if (isEmpty) {
                emptyCols.add(j);
            }
        }
        return emptyCols;
    }

    private static List<Point> findGalaxies(char[][] grid) {
        List<Point> galaxies = new ArrayList<>();
        for (int i = 0; i < grid.length; i++) {
            for (int j = 0; j < grid[0].length; j++) {
                if (grid[i][j] == '#') {
                    galaxies.add(new Point(i, j));
                }
            }
        }
        return galaxies;
    }

    private static long calculateDistance(Point p1, Point p2, List<Integer> emptyRows, List<Integer> emptyCols, int expansionFactor) {
        long rowDistance = Math.abs(p1.row - p2.row);
        long colDistance = Math.abs(p1.col - p2.col);

        for (int emptyRow : emptyRows) {
            if ((p1.row < emptyRow && emptyRow < p2.row) || (p2.row < emptyRow && emptyRow < p1.row)) {
                rowDistance += expansionFactor - 1;
            }
        }

        for (int emptyCol : emptyCols) {
            if ((p1.col < emptyCol && emptyCol < p2.col) || (p2.col < emptyCol && emptyCol < p1.col)) {
                colDistance += expansionFactor - 1;
            }
        }

        return rowDistance + colDistance;
    }

    static class Point {
        int row;
        int col;

        Point(int row, int col) {
            this.row = row;
            this.col = col;
        }
    }
}
