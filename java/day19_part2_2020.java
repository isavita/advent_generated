import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

public class Main {
    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String input = "";
        String line;
        while ((line = br.readLine()) != null) {
            input += line + "\n";
        }
        System.out.println(solve(input));
    }

    static int solve(String input) {
        String[] parts = input.split("\n\n");
        Map<Integer, Rule> graph = new HashMap<>();
        List<String> messages = new ArrayList<>(Arrays.asList(parts[1].split("\n")));

        for (String r : parts[0].split("\n")) {
            if (r.matches(".*[a-z]+.*")) {
                int num = Integer.parseInt(r.split(": ")[0]);
                String charStr = r.split(": \"")[1].substring(0, 1);
                graph.put(num, new Rule(charStr));
            } else {
                String[] split = r.split(": ");
                int key = Integer.parseInt(split[0]);
                Rule newRule = new Rule();
                for (String ruleNums : split[1].split(" \\| ")) {
                    String[] nums = ruleNums.split(" ");
                    List<Integer> option = new ArrayList<>();
                    for (String n : nums) {
                        option.add(Integer.parseInt(n));
                    }
                    newRule.options.add(option);
                }
                graph.put(key, newRule);
            }
        }

        fillInGraph(graph, 42);
        fillInGraph(graph, 31);

        String part42 = "(" + String.join("|", graph.get(42).resolved) + ")";
        String part31 = "(" + String.join("|", graph.get(31).resolved) + ")";
        String rule8String = "(" + part42 + ")+";

        int matchRuleZero = 0;
        for (String m : messages) {
            for (int i = 1; i < 10; i++) {
                Pattern pattern = Pattern.compile("^" + rule8String + "{" + i + "}" + part42 + "{" + i + "}" + part31 + "{" + i + "}$");
                if (pattern.matcher(m).matches()) {
                    matchRuleZero++;
                    break;
                }
            }
        }

        return matchRuleZero;
    }

    static List<String> fillInGraph(Map<Integer, Rule> graph, int entry) {
        if (!graph.get(entry).resolved.isEmpty()) {
            return new ArrayList<>(graph.get(entry).resolved);
        }

        for (List<Integer> option : graph.get(entry).options) {
            List<String> resolved = new ArrayList<>();
            resolved.add("");
            for (int entryPoint : option) {
                List<String> nestedResolveVals = fillInGraph(graph, entryPoint);
                List<String> newResolved = new ArrayList<>();
                for (String nextPiece : nestedResolveVals) {
                    for (String prev : resolved) {
                        newResolved.add(prev + nextPiece);
                    }
                }
                resolved = newResolved;
            }
            graph.get(entry).resolved.addAll(resolved);
        }

        return graph.get(entry).resolved;
    }

    static class Rule {
        List<String> resolved = new ArrayList<>();
        List<List<Integer>> options = new ArrayList<>();

        public Rule() {}

        public Rule(String charStr) {
            resolved.add(charStr);
        }
    }
}