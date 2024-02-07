
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class solution {
    
    static class Point {
        int x, y;
        
        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }
    }
    
    static Point[] directions = {
        new Point(-1, -1), new Point(0, -1), new Point(1, -1),
        new Point(-1, 0), /*new Point(0, 0),*/ new Point(1, 0),
        new Point(-1, 1), new Point(0, 1), new Point(1, 1)
    };
    
    public static void main(String[] args) {
        List<char[]> seatingArea = new ArrayList<>();
        
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                seatingArea.add(scanner.nextLine().toCharArray());
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("Error opening file: " + e);
            return;
        }
        
        boolean stabilized = false;
        while (!stabilized) {
            Object[] result = simulateSeatingPartTwo(seatingArea);
            seatingArea = (List<char[]>) result[0];
            stabilized = (boolean) result[1];
        }
        
        System.out.println(countOccupiedSeats(seatingArea));
    }
    
    public static Object[] simulateSeatingPartTwo(List<char[]> seatingArea) {
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
                        if (countVisibleOccupied(seatingArea, i, j) == 0) {
                            newSeatingArea.get(i)[j] = '#';
                            stabilized = false;
                        }
                        break;
                    case '#':
                        if (countVisibleOccupied(seatingArea, i, j) >= 5) {
                            newSeatingArea.get(i)[j] = 'L';
                            stabilized = false;
                        }
                        break;
                }
            }
        }
        
        return new Object[] { newSeatingArea, stabilized };
    }
    
    public static int countVisibleOccupied(List<char[]> seatingArea, int row, int col) {
        int count = 0;
        for (Point dir : directions) {
            for (int r = row + dir.y, c = col + dir.x; r >= 0 && r < seatingArea.size() && c >= 0 && c < seatingArea.get(0).length; r += dir.y, c += dir.x) {
                if (seatingArea.get(r)[c] == 'L') {
                    break;
                }
                if (seatingArea.get(r)[c] == '#') {
                    count++;
                    break;
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
