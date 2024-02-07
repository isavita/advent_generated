
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Solution {

    public static void main(String[] args) {
        int[] numbers = readInput("input.txt");
        int[] result = parseTree(numbers, 0);
        System.out.println(result[0]);
    }

    public static int[] readInput(String filename) {
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line = br.readLine();
            String[] parts = line.split(" ");
            int[] numbers = new int[parts.length];
            for (int i = 0; i < parts.length; i++) {
                numbers[i] = Integer.parseInt(parts[i]);
            }
            return numbers;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    public static int[] parseTree(int[] data, int index) {
        int childCount = data[index];
        int metaCount = data[index + 1];
        index += 2;

        int[] childValues = new int[childCount];
        for (int i = 0; i < childCount; i++) {
            int[] result = parseTree(data, index);
            childValues[i] = result[0];
            index = result[1];
        }

        int value = 0;
        if (childCount == 0) {
            for (int i = 0; i < metaCount; i++) {
                value += data[index + i];
            }
        } else {
            for (int i = 0; i < metaCount; i++) {
                int metadata = data[index + i];
                if (metadata <= childCount && metadata > 0) {
                    value += childValues[metadata - 1];
                }
            }
        }
        index += metaCount;

        return new int[]{value, index};
    }
}
