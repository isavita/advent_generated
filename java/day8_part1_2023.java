import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {
    static class Instruction {
        String left;
        String right;
    }

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String input = "";
        String line;
        while ((line = br.readLine()) != null) {
            input += line + "\n";
        }
        br.close();

        Pattern re = Pattern.compile("[A-Z]{3}");
        String[] lines = input.trim().split("\n");

        Map<String, Instruction> desertMap = new HashMap<>();

        for (int i = 2; i < lines.length; i++) {
            if (lines[i].trim().isEmpty()) {
                continue;
            }

            Matcher m = re.matcher(lines[i]);
            if (m.find()) {
                String key = m.group();
                m.find();
                String left = m.group();
                m.find();
                String right = m.group();
                desertMap.put(key, new Instruction());
                desertMap.get(key).left = left;
                desertMap.get(key).right = right;
            }
        }

        String current = "AAA";
        int steps = 0;

        while (!current.equals("ZZZ")) {
            for (char direction : lines[0].trim().toCharArray()) {
                if (direction == 'R') {
                    current = desertMap.get(current).right;
                } else if (direction == 'L') {
                    current = desertMap.get(current).left;
                }
                steps++;

                if (current.equals("ZZZ")) {
                    break;
                }
            }
        }

        System.out.println(steps);
    }
}