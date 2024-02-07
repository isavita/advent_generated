
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Solution {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            String input = scanner.nextLine();
            String[] startingNumbers = input.split(",");

            int[] spoken = new int[30000000];

            int lastSpoken = 0;
            for (int i = 0; i < startingNumbers.length; i++) {
                if (i == startingNumbers.length - 1) {
                    lastSpoken = Integer.parseInt(startingNumbers[i]);
                } else {
                    int num = Integer.parseInt(startingNumbers[i]);
                    spoken[num] = i + 1;
                }
            }

            for (int turn = startingNumbers.length + 1; turn <= 30000000; turn++) {
                int nextNumber = 0;
                if (spoken[lastSpoken] != 0) {
                    nextNumber = turn - 1 - spoken[lastSpoken];
                }
                spoken[lastSpoken] = turn - 1;
                lastSpoken = nextNumber;
            }

            System.out.println(lastSpoken);
        } catch (FileNotFoundException e) {
            System.out.println("Error reading file: " + e);
        }
    }
}
