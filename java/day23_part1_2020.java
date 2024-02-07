
import java.util.*;
import java.io.*;

public class solution {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            String input = scanner.nextLine();

            int[] cups = new int[input.length() + 1];
            int currentCup = 0;
            for (int i = 0; i < input.length(); i++) {
                int cup = Character.getNumericValue(input.charAt(i));
                if (i == 0) {
                    currentCup = cup;
                }
                if (i < input.length() - 1) {
                    int nextCup = Character.getNumericValue(input.charAt(i + 1));
                    cups[cup] = nextCup;
                }
            }
            int firstCup = Character.getNumericValue(input.charAt(0));
            int lastCup = Character.getNumericValue(input.charAt(input.length() - 1));
            cups[lastCup] = firstCup;

            for (int i = 0; i < 100; i++) {
                int pickup1 = cups[currentCup];
                int pickup2 = cups[pickup1];
                int pickup3 = cups[pickup2];

                cups[currentCup] = cups[pickup3];

                int destinationCup = currentCup - 1;
                if (destinationCup < 1) {
                    destinationCup = input.length();
                }
                while (destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3) {
                    destinationCup--;
                    if (destinationCup < 1) {
                        destinationCup = input.length();
                    }
                }

                cups[pickup3] = cups[destinationCup];
                cups[destinationCup] = pickup1;

                currentCup = cups[currentCup];
            }

            int cup = cups[1];
            while (cup != 1) {
                System.out.print(cup);
                cup = cups[cup];
                if (cup == 1) {
                    break;
                }
            }
            System.out.println();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}
