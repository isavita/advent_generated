
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        int[] numbers = readInput("input.txt");
        int[] result = parseTree(numbers, 0);
        System.out.println(result[0]);
    }

    public static int[] readInput(String filename) {
        int[] numbers = null;
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line = br.readLine();
            String[] parts = line.split(" ");
            numbers = new int[parts.length];
            for (int i = 0; i < parts.length; i++) {
                numbers[i] = Integer.parseInt(parts[i]);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return numbers;
    }

    public static int[] parseTree(int[] data, int index) {
        int childCount = data[index];
        int metaCount = data[index + 1];
        index += 2;

        int sum = 0;
        for (int i = 0; i < childCount; i++) {
            int[] childResult = parseTree(data, index);
            sum += childResult[0];
            index = childResult[1];
        }

        for (int i = 0; i < metaCount; i++) {
            sum += data[index + i];
        }
        index += metaCount;

        return new int[]{sum, index};
    }
}
