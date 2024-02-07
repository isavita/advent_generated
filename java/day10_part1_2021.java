
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

public class solution {

    public static void main(String[] args) {
        BufferedReader reader;
        int totalScore = 0;
        try {
            reader = new BufferedReader(new FileReader("input.txt"));
            String line = reader.readLine();
            while (line != null) {
                int[] result = checkLine(line);
                int score = result[0];
                boolean corrupted = result[1] == 1;
                if (corrupted) {
                    totalScore += score;
                }
                line = reader.readLine();
            }
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        System.out.println(totalScore);
    }

    public static int[] checkLine(String line) {
        Map<Character, Character> pairings = new HashMap<>();
        pairings.put(')', '(');
        pairings.put(']', '[');
        pairings.put('}', '{');
        pairings.put('>', '<');
        
        Map<Character, Integer> scores = new HashMap<>();
        scores.put(')', 3);
        scores.put(']', 57);
        scores.put('}', 1197);
        scores.put('>', 25137);
        
        Stack<Character> stack = new Stack<>();
        
        for (char c : line.toCharArray()) {
            switch (c) {
                case '(':
                case '[':
                case '{':
                case '<':
                    stack.push(c);
                    break;
                case ')':
                case ']':
                case '}':
                case '>':
                    if (stack.isEmpty() || stack.peek() != pairings.get(c)) {
                        return new int[] {scores.get(c), 1}; // corrupted line
                    }
                    stack.pop(); // pop from stack
                    break;
            }
        }
        
        return new int[] {0, 0}; // line is not corrupted
    }
}
