
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

    static class Network {
        String instructions;
        Map<String, String[]> nodes;

        Network(String instructions, Map<String, String[]> nodes) {
            this.instructions = instructions;
            this.nodes = nodes;
        }
    }

    static Network parseInput(List<String> input) {
        String instructions = input.get(0);
        Map<String, String[]> nodes = new HashMap<>();
        Pattern pattern = Pattern.compile("(\\w+) = \\((\\w+), (\\w+)\\)");
        for (int i = 2; i < input.size(); i++) {
            Matcher matcher = pattern.matcher(input.get(i));
            if (matcher.find()) {
                nodes.put(matcher.group(1), new String[]{matcher.group(2), matcher.group(3)});
            }
        }
        return new Network(instructions, nodes);
    }

    static long gcd(long a, long b) {
        while (b != 0) {
            long temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }

    static long lcm(long a, long b) {
        return (a * b) / gcd(a, b);
    }

    static long lcmSlice(List<Long> nums) {
        if (nums.isEmpty()) return 0;
        long res = nums.get(0);
        for (int i = 1; i < nums.size(); i++) {
            res = lcm(res, nums.get(i));
        }
        return res;
    }

    static long solve(List<String> input) {
        Network network = parseInput(input);
        List<String> starts = network.nodes.keySet().stream().filter(node -> node.endsWith("A")).collect(Collectors.toList());
        List<Long> steps = new ArrayList<>();
        int instructionsLength = network.instructions.length();
        for (String start : starts) {
            String element = start;
            long step = 0;
            while (!element.endsWith("Z")) {
                char instruction = network.instructions.charAt((int) (step % instructionsLength));
                element = instruction == 'L' ? network.nodes.get(element)[0] : network.nodes.get(element)[1];
                step++;
            }
            steps.add(step);
        }
        return lcmSlice(steps);
    }

    public static void main(String[] args) throws IOException {
        List<String> input = Files.readAllLines(Paths.get("input.txt"));
        System.out.println(solve(input));
    }
}
