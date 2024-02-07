
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            int totalCount = 0;
            HashMap<Character, Boolean> groupAnswers = new HashMap<>();

            String line;
            while ((line = reader.readLine()) != null) {
                if (line.equals("")) {
                    totalCount += groupAnswers.size();
                    groupAnswers.clear();
                } else {
                    for (char question : line.toCharArray()) {
                        groupAnswers.put(question, true);
                    }
                }
            }

            totalCount += groupAnswers.size();
            System.out.println(totalCount);

            reader.close();
        } catch (IOException e) {
            System.out.println("Error reading file: " + e);
        }
    }
}
