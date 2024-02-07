
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class solution {

    public static int countCombinations(List<Integer> containers, int target, int index) {
        if (target == 0) {
            return 1;
        }
        if (target < 0 || index >= containers.size()) {
            return 0;
        }
        return countCombinations(containers, target - containers.get(index), index + 1) +
                countCombinations(containers, target, index + 1);
    }

    public static void main(String[] args) {
        List<Integer> containers = new ArrayList<>();
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                int size = Integer.parseInt(scanner.nextLine());
                containers.add(size);
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("Error opening or reading file: " + e);
            return;
        }

        System.out.println(countCombinations(containers, 150, 0));
    }
}
