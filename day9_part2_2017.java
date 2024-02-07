
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            // Step 1: Read Input
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));

            // Step 2: Initialize Variables
            int score = 0;
            int depth = 0;
            boolean inGarbage = false;
            boolean cancelNext = false;
            int garbageCount = 0;

            // Step 3: Process Stream
            String line;
            while ((line = reader.readLine()) != null) {
                for (char ch : line.toCharArray()) {
                    if (cancelNext) {
                        cancelNext = false;
                        continue;
                    }

                    if (inGarbage) {
                        if (ch == '!') {
                            cancelNext = true;
                        } else if (ch == '>') {
                            inGarbage = false;
                        } else {
                            garbageCount++;
                        }
                    } else {
                        switch (ch) {
                            case '{':
                                depth++;
                                break;
                            case '}':
                                score += depth;
                                depth--;
                                break;
                            case '<':
                                inGarbage = true;
                                break;
                        }
                    }
                }
            }

            // Step 4: Print Results
            System.out.println(garbageCount);

            reader.close();
        } catch (IOException e) {
            System.out.println("File reading error: " + e.getMessage());
        }
    }
}
