
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        // Step 1: Read Input
        File file = new File("input.txt");
        try {
            Scanner scanner = new Scanner(file);
            
            // Step 2: Initialize Variables
            int score = 0;
            int depth = 0;
            boolean inGarbage = false;
            boolean cancelNext = false;

            // Step 3: Process Stream
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
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

            // Step 4: Print Score
            System.out.println(score);

        } catch (FileNotFoundException e) {
            System.out.println("File reading error" + e);
        }
    }
}
