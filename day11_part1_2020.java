
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static void main(String[] args) {
        List<char[]> seatingArea = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                seatingArea.add(line.toCharArray());
            }
        } catch (IOException e) {
            System.err.println("Error opening file: " + e.getMessage());
            return;
        }

        boolean stabilized = false;
        while (!stabilized) {
            Object[] result = simulateSeating(seatingArea);
            seatingArea = (List<char[]>) result[0];
            stabilized = (boolean) result[1];
        }

        System.out.println(countOccupiedSeats(seatingArea));
    }

    public static Object[] simulateSeating(List<char[]> seatingArea) {
        int rows = seatingArea.size();
        int cols = seatingArea.get(0).length;
        List<char[]> newSeatingArea = new ArrayList<>();
        for (char[] row : seatingArea) {
            newSeatingArea.add(row.clone());
        }
        boolean stabilized = true;

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                char seat = seatingArea.get(i)[j];
                switch (seat) {
                    case 'L':
                        if (countAdjacentOccupied(seatingArea, i, j) == 0) {
                            newSeatingArea.get(i)[j] = '#';
                            stabilized = false;
                        }
                        break;
                    case '#':
                        if (countAdjacentOccupied(seatingArea, i, j) >= 4) {
                            newSeatingArea.get(i)[j] = 'L';
                            stabilized = false;
                        }
                        break;
                }
            }
        }

        return new Object[]{newSeatingArea, stabilized};
    }

    public static int countAdjacentOccupied(List<char[]> seatingArea, int row, int col) {
        int count = 0;
        for (int i = row - 1; i <= row + 1; i++) {
            for (int j = col - 1; j <= col + 1; j++) {
                if (i == row && j == col) {
                    continue;
                }
                if (i >= 0 && i < seatingArea.size() && j >= 0 && j < seatingArea.get(0).length) {
                    if (seatingArea.get(i)[j] == '#') {
                        count++;
                    }
                }
            }
        }
        return count;
    }

    public static int countOccupiedSeats(List<char[]> seatingArea) {
        int count = 0;
        for (char[] row : seatingArea) {
            for (char seat : row) {
                if (seat == '#') {
                    count++;
                }
            }
        }
        return count;
    }
}
