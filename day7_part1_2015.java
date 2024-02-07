
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            StringBuilder input = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                input.append(line).append("\n");
            }
            reader.close();

            System.out.println(someAssemblyRequired(input.toString()));
        } catch (IOException e) {
            System.out.println("Error reading from file: " + e.getMessage());
        }
    }

    public static int someAssemblyRequired(String input) {
        Map<String, String> wireToRule = new HashMap<>();

        for (String inst : input.split("\n")) {
            String[] parts = inst.split(" -> ");
            wireToRule.put(parts[1], parts[0]);
        }

        return memoDFS(wireToRule, "a", new HashMap<>());
    }

    public static int memoDFS(Map<String, String> graph, String entry, Map<String, Integer> memo) {
        if (memo.containsKey(entry)) {
            return memo.get(entry);
        }

        if (Pattern.compile("[0-9]").matcher(entry).find()) {
            return Integer.parseInt(entry);
        }

        String sourceRule = graph.get(entry);
        String[] parts = sourceRule.split(" ");

        int result = 0;
        switch (parts.length) {
            case 1:
                result = memoDFS(graph, parts[0], memo);
                break;
            case 2:
                if (parts[0].equals("NOT")) {
                    int start = memoDFS(graph, parts[1], memo);
                    result = 65535 ^ start;
                }
                break;
            case 3:
                int val1 = memoDFS(graph, parts[0], memo);
                int val2 = memoDFS(graph, parts[2], memo);
                switch (parts[1]) {
                    case "AND":
                        result = val1 & val2;
                        break;
                    case "OR":
                        result = val1 | val2;
                        break;
                    case "LSHIFT":
                        result = val1 << val2;
                        break;
                    case "RSHIFT":
                        result = val1 >> val2;
                        break;
                }
                break;
        }

        memo.put(entry, result);
        return result;
    }
}
