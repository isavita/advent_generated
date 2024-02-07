
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static void main(String[] args) {
        List<String> input = new ArrayList<>();
        List<String> steps = new ArrayList<>();
        List<List<Character>> stacks = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            int count = 0;
            while ((line = br.readLine()) != null) {
                if (line.isEmpty()) {
                    count++;
                    continue;
                }
                if (count == 0) {
                    input.add(line);
                } else if (count == 1) {
                    steps.add(line);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        for (int i = 0; i < (input.get(0).length() + 1) / 4; i++) {
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

        System.out.println(move(stacks, steps));
    }

    public static String move(List<List<Character>> st, List<String> steps) {
        List<List<Character>> stacks = new ArrayList<>();
        for (List<Character> stack : st) {
            List<Character> temp = new ArrayList<>();
            for (int j = 0; j < stack.size(); j++) {
                temp.add(stack.get(stack.size() - j - 1));
            }
            stacks.add(temp);
        }

        for (String step : steps) {
            int n, from, to;
            String[] tokens = step.split(" ");
            n = Integer.parseInt(tokens[1]);
            from = Integer.parseInt(tokens[3]) - 1;
            to = Integer.parseInt(tokens[5]) - 1;

            stacks.get(to).addAll(stacks.get(from).subList(stacks.get(from).size() - n, stacks.get(from).size()));
            stacks.get(from).subList(stacks.get(from).size() - n, stacks.get(from).size()).clear();
        }

        StringBuilder sb = new StringBuilder();
        for (List<Character> stack : stacks) {
            sb.append(stack.get(stack.size() - 1));
        }

        return sb.toString();
    }
}
