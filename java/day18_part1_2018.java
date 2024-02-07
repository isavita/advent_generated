
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class solution {
    private static final char Open = '.';
    private static final char Trees = '|';
    private static final char Lumberyard = '#';
    private static final int Size = 50;

    public static void main(String[] args) {
        char[][] grid = readInput("input.txt");

        for (int minute = 0; minute < 10; minute++) {
            grid = transform(grid);
        }

        int[] resources = countResources(grid);
        System.out.println(resources[0] * resources[1]);
    }

    public static char[][] readInput(String filename) {
        try {
            Scanner scanner = new Scanner(new File(filename));
            char[][] grid = new char[Size][Size];
            int row = 0;
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                for (int i = 0; i < line.length(); i++) {
                    grid[row][i] = line.charAt(i);
                }
                row++;
            }
            scanner.close();
            return grid;
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            return null;
        }
    }

    public static char[][] transform(char[][] grid) {
        char[][] newGrid = new char[Size][Size];
        for (int i = 0; i < Size; i++) {
            for (int j = 0; j < Size; j++) {
                newGrid[i][j] = nextAcreState(grid, i, j);
            }
        }
        return newGrid;
    }

    public static char nextAcreState(char[][] grid, int i, int j) {
        switch (grid[i][j]) {
            case Open:
                if (countAdjacent(grid, i, j, Trees) >= 3) {
                    return Trees;
                }
                break;
            case Trees:
                if (countAdjacent(grid, i, j, Lumberyard) >= 3) {
                    return Lumberyard;
                }
                break;
            case Lumberyard:
                if (countAdjacent(grid, i, j, Lumberyard) >= 1 && countAdjacent(grid, i, j, Trees) >= 1) {
                    return Lumberyard;
                }
                return Open;
        }
        return grid[i][j];
    }

    public static int countAdjacent(char[][] grid, int i, int j, char acreType) {
        int count = 0;
        for (int x = -1; x <= 1; x++) {
            for (int y = -1; y <= 1; y++) {
                if (x == 0 && y == 0) {
                    continue;
                }
                if (i + x >= 0 && i + x < Size && j + y >= 0 && j + y < Size && grid[i + x][j + y] == acreType) {
                    count++;
                }
            }
        }
        return count;
    }

    public static int[] countResources(char[][] grid) {
        int wooded = 0;
        int lumberyards = 0;
        for (int i = 0; i < Size; i++) {
            for (int j = 0; j < Size; j++) {
                switch (grid[i][j]) {
                    case Trees:
                        wooded++;
                        break;
                    case Lumberyard:
                        lumberyards++;
                        break;
                }
            }
        }
        return new int[]{wooded, lumberyards};
    }
}
