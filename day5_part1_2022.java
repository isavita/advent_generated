
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static void main(String[] args) {
        String content = readAll("input.txt");
        String[] s = content.split("\n\n");
        String[] input = s[0].split("\n");
        List<List<Character>> stacks = new ArrayList<>();
        for (int i = 0; i < (input[0].length() + 1) / 4; i++) {
            stacks.add(new ArrayList<>());
        }
        for (String line : input) {
            for (int i = 0; i < line.length(); i++) {
                char b = line.charAt(i);
                if (b >= 'A' && b <= 'Z') {
                    stacks.get((i - 1) / 4).add(b);
                }
            }
        }

        String[] steps = s[1].split("\n");
        System.out.println(move(stacks, steps));
    }

    public static String move(List<List<Character>> st, String[] steps) {
        List<List<Character>> stacks = new ArrayList<>();
        for (List<Character> stack : st) {
            List<Character> newStack = new ArrayList<>();
            for (int j = stack.size() - 1; j >= 0; j--) {
                newStack.add(stack.get(j));
            }
            stacks.add(newStack);
        }

        for (String step : steps) {
            int n, from, to;
            String[] parts = step.split(" ");
            n = Integer.parseInt(parts[1]);
            from = Integer.parseInt(parts[3]) - 1;
            to = Integer.parseInt(parts[5]) - 1;
            for (int i = 0; i < n; i++) {
                stacks.get(to).add(stacks.get(from).get(stacks.get(from).size() - 1));
                stacks.get(from).remove(stacks.get(from).size() - 1);
            }
        }

        StringBuilder result = new StringBuilder();
        for (List<Character> stack : stacks) {
            result.append(stack.get(stack.size() - 1));
        }
        return result.toString();
    }

    public static String readAll(String path) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(path));
            StringBuilder content = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                content.append(line).append("\n");
            }
            reader.close();
            return content.toString().trim();
        } catch (IOException e) {
            e.printStackTrace();
            return "";
        }
    }
}
