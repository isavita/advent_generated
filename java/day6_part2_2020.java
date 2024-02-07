
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            int totalCount = 0;
            HashMap<Character, Integer> groupAnswers = new HashMap<>();
            int groupSize = 0;
            String line;

            while ((line = reader.readLine()) != null) {
                if (line.isEmpty()) {
                    for (int count : groupAnswers.values()) {
                        if (count == groupSize) {
                            totalCount++;
                        }
                    }
                    groupAnswers.clear();
                    groupSize = 0;
                } else {
                    groupSize++;
                    for (char question : line.toCharArray()) {
                        groupAnswers.put(question, groupAnswers.getOrDefault(question, 0) + 1);
                    }
                }
            }

            for (int count : groupAnswers.values()) {
                if (count == groupSize) {
                    totalCount++;
                }
            }

            System.out.println(totalCount);
            reader.close();
        } catch (IOException e) {
            System.out.println("Error opening file: " + e);
        }
    }
}
