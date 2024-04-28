import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String line;
        int[] xValues = {1};
        while ((line = br.readLine()) != null) {
            if (line.equals("noop")) {
                xValues = append(xValues, xValues[xValues.length - 1]);
            } else {
                int n = Integer.parseInt(line.split(" ")[1]);
                xValues = append(xValues, xValues[xValues.length - 1]);
                xValues = append(xValues, xValues[xValues.length - 1] + n);
            }
        }

        boolean[][] grid = new boolean[40][6];
        for (int i = 0; i < xValues.length; i++) {
            int crtX = i % 40;
            int crtY = i / 40;
            if (Math.abs(crtX - xValues[i]) <= 1) {
                grid[crtX][crtY] = true;
            }
        }

        for (int y = 0; y < 6; y++) {
            for (int col = 0; col < 40; col++) {
                System.out.print(grid[col][y] ? "#" : ".");
            }
            System.out.println();
        }
    }

    public static int[] append(int[] arr, int val) {
        int[] newArr = new int[arr.length + 1];
        System.arraycopy(arr, 0, newArr, 0, arr.length);
        newArr[arr.length] = val;
        return newArr;
    }
}