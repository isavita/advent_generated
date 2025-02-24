
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.*;

public class JsonSum {

    public static void main(String[] args) {
        try {
            String input = readFile("input.txt");
            System.out.println("Part 1 Sum: " + sumNumbersPart1(input));
            System.out.println("Part 2 Sum: " + sumNumbersPart2(input));
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        }
    }

    // Helper function to read the file content
    static String readFile(String filename) throws IOException {
        StringBuilder content = new StringBuilder();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                content.append(line);
            }
        }
        return content.toString();
    }

    // Part 1: Sum all numbers in the JSON string
    static int sumNumbersPart1(String json) {
        int sum = 0;
        Matcher matcher = Pattern.compile("-?\\d+").matcher(json); // Matches integers (including negatives)
        while (matcher.find()) {
            sum += Integer.parseInt(matcher.group());
        }
        return sum;
    }


    // Part 2: Sum numbers, ignoring objects with a "red" property
    static int sumNumbersPart2(String json) {
        Object parsedJson = parseJson(json);
        return sumNumbersRecursive(parsedJson);
    }

    static int sumNumbersRecursive(Object obj) {
        if (obj instanceof Integer) {
            return (Integer) obj;
        } else if (obj instanceof List) {
            int sum = 0;
            for (Object item : (List<?>) obj) {
                sum += sumNumbersRecursive(item);
            }
            return sum;
        } else if (obj instanceof Map) {
            Map<?, ?> map = (Map<?, ?>) obj;
            if (map.containsValue("red")) {
                return 0;
            }
            int sum = 0;
            for (Object value : map.values()) {
                sum += sumNumbersRecursive(value);
            }
            return sum;
        }
        return 0; // For strings and other types
    }

    // Simplified JSON parsing (handles nested objects and arrays, integers and strings)
    static Object parseJson(String json) {
        json = json.trim();

        if (json.startsWith("[")) {  // Array
            List<Object> list = new ArrayList<>();
            json = json.substring(1, json.length() - 1).trim(); // Remove brackets
            if(json.isEmpty()) return list;

            int bracketCount = 0;
            int braceCount = 0;
            int start = 0;
            for (int i = 0; i < json.length(); i++) {
                char c = json.charAt(i);
                if (c == '[') {
                    bracketCount++;
                } else if (c == ']') {
                    bracketCount--;
                } else if (c == '{') {
                    braceCount++;
                } else if (c == '}') {
                    braceCount--;
                } else if (c == ',' && bracketCount == 0 && braceCount == 0) {
                    list.add(parseJson(json.substring(start, i)));
                    start = i + 1;
                }
            }
             list.add(parseJson(json.substring(start)));  //last element

            return list;
        } else if (json.startsWith("{")) {  // Object
            Map<String, Object> map = new HashMap<>();
            json = json.substring(1, json.length() - 1).trim(); // Remove braces
             if(json.isEmpty()) return map;

            int bracketCount = 0;
            int braceCount = 0;
            int start = 0;
             List<String> pairs = new ArrayList<>();

            for (int i = 0; i < json.length(); i++) {
                char c = json.charAt(i);
                if (c == '[') {
                    bracketCount++;
                } else if (c == ']') {
                    bracketCount--;
                } else if (c == '{') {
                    braceCount++;
                } else if (c == '}') {
                    braceCount--;
                } else if (c == ',' && bracketCount == 0 && braceCount == 0) {
                    pairs.add(json.substring(start, i));
                    start = i + 1;
                }
            }
            pairs.add(json.substring(start));

            for(String pairStr : pairs){
                String[] parts = pairStr.split(":", 2); // Split only on the first colon

                if(parts.length == 2){ //ensure key-value
                    String key = parts[0].trim().replaceAll("\"", ""); //remove quotes
                    String valueStr = parts[1].trim();
                    map.put(key,parseJson(valueStr));
                }

            }
            return map;


        } else if (json.matches("-?\\d+")) {  // Integer
            return Integer.parseInt(json);
        } else { // Assume string (remove quotes if present)
            return json.replaceAll("\"", "");
        }
    }
}
