
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

        PartInterval(int min, int max) {
            intervals.put('x', new Interval(min, max));
            intervals.put('m', new Interval(min, max));
            intervals.put('a', new Interval(min, max));
            intervals.put('s', new Interval(min, max));
        }

        PartInterval(PartInterval other) {
            this.intervals = new HashMap<>(other.intervals);
        }
    }

    static class Workflow {
        List<Rule> rules = new ArrayList<>();
    }

    static class Workflows {
        Map<String, Workflow> workflows = new HashMap<>();
    }

    private static Workflows parseWorkflows(List<String> input) {
        Workflows workflows = new Workflows();
        int i = 0;
        for (; !input.get(i).isEmpty(); i++) {
            String line = input.get(i);
            int idx = line.indexOf("{");
            String workflowName = line.substring(0, idx);
            String rulesStr = line.substring(idx + 1, line.length() - 1);
            String[] rulesArr = rulesStr.split(",");
            Workflow workflow = new Workflow();
            for (String ruleStr : rulesArr) {
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
            workflows.workflows.put(workflowName, workflow);
        }
        return workflows;
    }

    private static long applyWorkflowInterval(PartInterval partInterval, Workflows workflows, String workflowName) {
        if (workflowName.equals("A")) {
            long res = 1;
            for (Interval interval : partInterval.intervals.values()) {
                res *= (long) (interval.end - interval.start + 1);
            }
            return res;
        }
        if (workflowName.equals("R")) {
            return 0;
        }

        long res = 0;
        Workflow workflow = workflows.workflows.get(workflowName);
        for (Rule rule : workflow.rules) {
            Interval ratingInterval = partInterval.intervals.get(rule.category);
            Interval validRatingInterval;
            Interval invalidRatingInterval;

            if (rule.operator == '>') {
                invalidRatingInterval = new Interval(ratingInterval.start, rule.num);
                validRatingInterval = new Interval(rule.num + 1, ratingInterval.end);
            } else if (rule.operator == '<') {
                validRatingInterval = new Interval(ratingInterval.start, rule.num - 1);
                invalidRatingInterval = new Interval(rule.num, ratingInterval.end);
            } else {
                validRatingInterval = ratingInterval;
                invalidRatingInterval = new Interval(0,0);
            }

            PartInterval newPart = new PartInterval(partInterval);
            if(rule.operator != 0){
                newPart.intervals.put(rule.category, validRatingInterval);
            }
            res += applyWorkflowInterval(newPart, workflows, rule.workflowName);

            if(rule.operator != 0){
                partInterval.intervals.put(rule.category, invalidRatingInterval);
            }
        }
        return res;
    }

    public static long solve(List<String> input) {
        Workflows workflows = parseWorkflows(input);
        int minRating = 1;
        int maxRating = 4000;
        PartInterval partInterval = new PartInterval(minRating, maxRating);
        return applyWorkflowInterval(partInterval, workflows, "in");
    }

    public static void main(String[] args) throws IOException {
        List<String> input = Files.readAllLines(Paths.get("input.txt"));
        System.out.println(solve(input));
    }
}
