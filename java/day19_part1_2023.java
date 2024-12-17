
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

class Solution {

    static class Rule {
        char category;
        char operator;
        int num;
        String workflowName;

        Rule(char category, char operator, int num, String workflowName) {
            this.category = category;
            this.operator = operator;
            this.num = num;
            this.workflowName = workflowName;
        }

        Rule(String workflowName) {
            this.workflowName = workflowName;
        }
    }

    static class Part {
        int x, m, a, s;

        Part(int x, int m, int a, int s) {
            this.x = x;
            this.m = m;
            this.a = a;
            this.s = s;
        }

        int get(char category) {
            return switch (category) {
                case 'x' -> x;
                case 'm' -> m;
                case 'a' -> a;
                case 's' -> s;
                default -> 0;
            };
        }
    }

    static class Interval {
        int start;
        int end;

        Interval(int start, int end) {
            this.start = start;
            this.end = end;
        }
    }

    static class PartInterval {
        Map<Character, Interval> intervals = new HashMap<>();

        PartInterval(int start, int end) {
            intervals.put('x', new Interval(start, end));
            intervals.put('m', new Interval(start, end));
            intervals.put('a', new Interval(start, end));
            intervals.put('s', new Interval(start, end));
        }

        PartInterval(PartInterval other) {
            this.intervals = new HashMap<>(other.intervals);
        }
    }

    static class Workflow {
        List<Rule> rules = new ArrayList<>();
    }

    static Map<String, Workflow> workflows = new HashMap<>();
    static List<Part> parts = new ArrayList<>();

    public static void main(String[] args) throws IOException {
        List<String> input = Files.readAllLines(Paths.get("input.txt"));
        parseInput(input);
        System.out.println(solve());
    }

    static void parseInput(List<String> input) {
        int i = 0;
        for (; !input.get(i).isEmpty(); i++) {
            parseWorkflow(input.get(i));
        }

        for (i = i + 1; i < input.size(); i++) {
            parsePart(input.get(i));
        }
    }

    static void parseWorkflow(String line) {
        int idx = line.indexOf("{");
        String workflowName = line.substring(0, idx);
        String rulesStr = line.substring(idx + 1, line.length() - 1);
        Workflow workflow = new Workflow();
        for (String ruleStr : rulesStr.split(",")) {
            int colonIdx = ruleStr.indexOf(":");
            if (colonIdx == -1) {
                workflow.rules.add(new Rule(ruleStr));
            } else {
                char category = ruleStr.charAt(0);
                char operator = ruleStr.charAt(1);
                int num = Integer.parseInt(ruleStr.substring(2, colonIdx));
                String workflowNameRule = ruleStr.substring(colonIdx + 1);
                workflow.rules.add(new Rule(category, operator, num, workflowNameRule));
            }
        }
        workflows.put(workflowName, workflow);
    }

    static void parsePart(String line) {
        Pattern pattern = Pattern.compile("\\{x=(\\d+),m=(\\d+),a=(\\d+),s=(\\d+)\\}");
        Matcher matcher = pattern.matcher(line);
        if (matcher.find()) {
            int x = Integer.parseInt(matcher.group(1));
            int m = Integer.parseInt(matcher.group(2));
            int a = Integer.parseInt(matcher.group(3));
            int s = Integer.parseInt(matcher.group(4));
            parts.add(new Part(x, m, a, s));
        }
    }

    static boolean applyWorkflow(Part part, String workflowName) {
        if (workflowName.equals("A")) {
            return true;
        }
        if (workflowName.equals("R")) {
            return false;
        }

        Workflow workflow = workflows.get(workflowName);
        for (Rule rule : workflow.rules) {
            int rating = part.get(rule.category);
            boolean isValid = true;
            switch (rule.operator) {
                case '>':
                    isValid = rating > rule.num;
                    break;
                case '<':
                    isValid = rating < rule.num;
                    break;
                default:
                    break;
            }
            if (isValid) {
                return applyWorkflow(part, rule.workflowName);
            }
        }
        return false;
    }

    static int solve() {
        String startWorkflow = "in";
        int res = 0;
        for (Part part : parts) {
            if (applyWorkflow(part, startWorkflow)) {
                res += part.x + part.m + part.a + part.s;
            }
        }
        return res;
    }
}
