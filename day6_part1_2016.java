
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            StringBuilder sb = new StringBuilder();
            while ((line = reader.readLine()) != null) {
                sb.append(line).append("\n");
            }
            reader.close();

            String[] messages = sb.toString().split("\n");
            String correctedMessage = getCorrectedMessage(messages);
            System.out.println(correctedMessage);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static String getCorrectedMessage(String[] messages) {
        if (messages.length == 0) {
            return "";
        }
        int messageLength = messages[0].length();
        Map<Character, Integer>[] count = new HashMap[messageLength];

        for (int i = 0; i < count.length; i++) {
            count[i] = new HashMap<>();
        }

        for (String message : messages) {
            for (int j = 0; j < message.length(); j++) {
                char c = message.charAt(j);
                count[j].put(c, count[j].getOrDefault(c, 0) + 1);
            }
        }

        StringBuilder correctedMessage = new StringBuilder();
        for (Map<Character, Integer> charCount : count) {
            correctedMessage.append(getMostCommonChar(charCount));
        }

        return correctedMessage.toString();
    }

    public static char getMostCommonChar(Map<Character, Integer> count) {
        char maxChar = 0;
        int maxCount = 0;
        for (Map.Entry<Character, Integer> entry : count.entrySet()) {
            char c = entry.getKey();
            int cnt = entry.getValue();
            if (cnt > maxCount) {
                maxCount = cnt;
                maxChar = c;
            }
        }
        return maxChar;
    }
}
