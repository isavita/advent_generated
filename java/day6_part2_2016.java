
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class solution {
    public static void main(String[] args) {
        List<String> messages = new ArrayList<>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            while ((line = reader.readLine()) != null) {
                messages.add(line);
            }
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        String originalMessage = getOriginalMessage(messages);
        System.out.println(originalMessage);
    }

    public static String getOriginalMessage(List<String> messages) {
        if (messages.size() == 0) {
            return "";
        }
        int messageLength = messages.get(0).length();
        List<Map<Character, Integer>> count = new ArrayList<>();
        for (int i = 0; i < messageLength; i++) {
            count.add(new HashMap<>());
        }

        for (String message : messages) {
            for (int j = 0; j < message.length(); j++) {
                char charAtJ = message.charAt(j);
                count.get(j).put(charAtJ, count.get(j).getOrDefault(charAtJ, 0) + 1);
            }
        }

        StringBuilder originalMessage = new StringBuilder();
        for (Map<Character, Integer> charCount : count) {
            originalMessage.append(getLeastCommonChar(charCount));
        }

        return originalMessage.toString();
    }

    public static char getLeastCommonChar(Map<Character, Integer> count) {
        char minChar = '\0';
        int minCount = Integer.MAX_VALUE;
        for (Map.Entry<Character, Integer> entry : count.entrySet()) {
            if (entry.getValue() < minCount) {
                minCount = entry.getValue();
                minChar = entry.getKey();
            }
        }
        return minChar;
    }
}
