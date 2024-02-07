
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        int totalElves = readInput("input.txt");
        int winner = findWinningElf(totalElves);
        System.out.println(winner);
    }

    public static int readInput(String filename) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filename));
            int totalElves = Integer.parseInt(reader.readLine());
            reader.close();
            return totalElves;
        } catch (IOException e) {
            e.printStackTrace();
            return -1;
        }
    }

    public static int findWinningElf(int totalElves) {
        int highestPowerOfTwo = 1;
        while (highestPowerOfTwo * 2 <= totalElves) {
            highestPowerOfTwo *= 2;
        }
        return (totalElves - highestPowerOfTwo) * 2 + 1;
    }
}
