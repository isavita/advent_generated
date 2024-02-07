
import java.io.BufferedReader;
import java.io.FileReader;

public class solution {
    public static String react(String polymer) {
        for (int i = 0; i < polymer.length() - 1; i++) {
            if (polymer.charAt(i) != polymer.charAt(i + 1) &&
                    (polymer.charAt(i) + 32 == polymer.charAt(i + 1) ||
                            polymer.charAt(i) - 32 == polymer.charAt(i + 1))) {
                return react(polymer.substring(0, i) + polymer.substring(i + 2));
            }
        }
        return polymer;
    }

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String polymer = reader.readLine();

            String result = react(polymer);
            System.out.println(result.length());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
