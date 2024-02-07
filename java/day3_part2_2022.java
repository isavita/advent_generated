
import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class solution {
    public static int itemPriority(char item) {
        if (item >= 'a' && item <= 'z') {
            return (int) (item - 'a') + 1;
        }
        return (int) (item - 'A') + 27;
    }

    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);

            int sum = 0;
            int groupLineCounter = 0;
            Map<Character, Integer>[] groupItems = new HashMap[3];

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                Map<Character, Integer> itemsMap = new HashMap<>();
                for (char item : line.toCharArray()) {
                    itemsMap.put(item, itemsMap.getOrDefault(item, 0) + 1);
                }
                groupItems[groupLineCounter] = itemsMap;
                groupLineCounter++;

                if (groupLineCounter == 3) {
                    Map<Character, Integer> commonItems = new HashMap<>();
                    for (char item : groupItems[0].keySet()) {
                        if (groupItems[1].containsKey(item) && groupItems[2].containsKey(item)) {
                            commonItems.put(item, commonItems.getOrDefault(item, 0) + 1);
                        }
                    }
                    for (char item : commonItems.keySet()) {
                        sum += itemPriority(item);
                        break; // Since we need only one common item per group
                    }
                    groupLineCounter = 0;
                }
            }

            System.out.println(sum);
            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("Error opening file: " + e.getMessage());
        }
    }
}
