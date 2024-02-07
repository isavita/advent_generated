
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {

    public static void main(String[] args) {
        String initialSequence = readInput("input.txt");
        if (initialSequence.equals("")) {
            System.out.println("Error reading input");
            return;
        }

        String result = lookAndSay(initialSequence, 40);
        System.out.println(result.length());
    }

    public static String readInput(String filename) {
        StringBuilder content = new StringBuilder();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            if ((line = br.readLine()) != null) {
                content.append(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return content.toString();
    }

    public static String lookAndSay(String sequence, int iterations) {
        for (int i = 0; i < iterations; i++) {
            sequence = nextSequence(sequence);
        }
        return sequence;
    }

    public static String nextSequence(String sequence) {
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < sequence.length();) {
            int count = 1;
            char digit = sequence.charAt(i);
            for (int j = i + 1; j < sequence.length() && sequence.charAt(j) == digit; j++) {
                count++;
            }
            result.append(count).append(digit);
            i += count;
        }
        return result.toString();
    }
}
