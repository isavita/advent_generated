
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String input = reader.readLine().trim();
            int target = Integer.parseInt(input) / 11;

            int[] houses = new int[target + 1];

            for (int elf = 1; elf <= target; elf++) {
                for (int house = elf; house <= elf * 50 && house <= target; house += elf) {
                    houses[house] += elf;
                }
            }

            for (int houseNumber = 0; houseNumber < houses.length; houseNumber++) {
                if (houses[houseNumber] >= target) {
                    System.out.println(houseNumber);
                    break;
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
