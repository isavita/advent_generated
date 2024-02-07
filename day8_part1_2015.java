
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            int totalDiff = 0;
            String line;

            while ((line = reader.readLine()) != null) {
                int codeLength = line.length();
                int memoryLength = calculateMemoryLength(line);
                totalDiff += codeLength - memoryLength;
            }

            System.out.println(totalDiff);
            reader.close();
        } catch (IOException e) {
            System.out.println("Error reading file: " + e);
        }
    }

    public static int calculateMemoryLength(String s) {
        int length = 0;
        boolean inEscape = false;
        int hexCount = 0;

        for (int i = 1; i < s.length() - 1; i++) {
            if (hexCount > 0) {
                hexCount--;
            } else if (inEscape) {
                if (s.charAt(i) == 'x') {
                    hexCount = 2;
                }
                inEscape = false;
                length++;
            } else if (s.charAt(i) == '\\') {
                inEscape = true;
            } else {
                length++;
            }
        }
        return length;
    }
}
