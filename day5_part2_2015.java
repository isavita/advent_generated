
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line = reader.readLine();
            int nice = 0;

            while (line != null) {
                if (passesRule1(line) && passesRule2(line)) {
                    nice++;
                }
                line = reader.readLine();
            }

            System.out.println(nice);
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static boolean passesRule1(String line) {
        for (int i = 0; i < line.length() - 2; i++) {
            String toMatch = line.substring(i, i + 2);
            for (int j = i + 2; j < line.length() - 1; j++) {
                if (line.substring(j, j + 2).equals(toMatch)) {
                    return true;
                }
            }
        }
        return false;
    }

    private static boolean passesRule2(String line) {
        for (int i = 0; i < line.length() - 2; i++) {
            if (line.charAt(i) == line.charAt(i + 2)) {
                return true;
            }
        }
        return false;
    }
}
