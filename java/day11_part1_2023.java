
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class CosmicExpansion {

    public static void main(String[] args) {
        try {
            List<String> lines = readInput("input.txt");
            long sumOfShortestPaths = calculateSumOfShortestPaths(lines);
            System.out.println(sumOfShortestPaths);
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

    private static long calculateSumOfShortestPaths(List<String> lines) {
        List<Integer> emptyRows = findEmptyRows(lines);
        List<Integer> emptyCols = findEmptyCols(lines);
        List<Point> galaxies = findGalaxies(lines);

        long sumOfPaths = 0;
        for (int i = 0; i < galaxies.size(); i++) {
            for (int j = i + 1; j < galaxies.size(); j++) {
                sumOfPaths += calculateManhattanDistance(galaxies.get(i), galaxies.get(j), emptyRows, emptyCols);
            }
        }
        return sumOfPaths;
    }

    private static List<Integer> findEmptyRows(List<String> lines) {
        List<Integer> emptyRows = new ArrayList<>();
        for (int i = 0; i < lines.size(); i++) {
            if (!lines.get(i).contains("#")) {
                emptyRows.add(i);
            }
        }
        return emptyRows;
    }

    private static List<Integer> findEmptyCols(List<String> lines) {
        List<Integer> emptyCols = new ArrayList<>();
        for (int j = 0; j < lines.get(0).length(); j++) {
            boolean isEmpty = true;
            for (String line : lines) {
                if (line.charAt(j) == '#') {
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

    private static List<Point> findGalaxies(List<String> lines) {
        List<Point> galaxies = new ArrayList<>();
        for (int i = 0; i < lines.size(); i++) {
            for (int j = 0; j < lines.get(i).length(); j++) {
                if (lines.get(i).charAt(j) == '#') {
                    galaxies.add(new Point(i, j));
                }
            }
        }
        return galaxies;
    }

    private static long calculateManhattanDistance(Point p1, Point p2, List<Integer> emptyRows, List<Integer> emptyCols) {
        long rowDistance = Math.abs(p1.row - p2.row);
        long colDistance = Math.abs(p1.col - p2.col);

        for (int emptyRow : emptyRows) {
            if ((p1.row < emptyRow && emptyRow < p2.row) || (p2.row < emptyRow && emptyRow < p1.row)) {
                rowDistance++;
            }
        }

        for (int emptyCol : emptyCols) {
            if ((p1.col < emptyCol && emptyCol < p2.col) || (p2.col < emptyCol && emptyCol < p1.col)) {
                colDistance++;
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
