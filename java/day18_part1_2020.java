
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.Deque;

public class Main {

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            long sum = 0;
            String line;
            while ((line = br.readLine()) != null) {
                sum += evaluate(line);
            }
            System.out.println(sum);
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        }
    }

    private static long evaluate(String expression) {
        Deque<Character> ops = new ArrayDeque<>();
        Deque<Long> vals = new ArrayDeque<>();
        
        for (int i = 0; i < expression.length(); i++) {
            char token = expression.charAt(i);
            if (token == ' ') continue;
            if (token == '(') {
                ops.push(token);
            } else if (token == '+' || token == '*') {
                while (!ops.isEmpty() && ops.peek() != '(') {
                    vals.push(applyOp(ops.pop(), vals.pop(), vals.pop()));
                }
                ops.push(token);
            } else if (token == ')') {
                while (ops.peek() != '(') {
                    vals.push(applyOp(ops.pop(), vals.pop(), vals.pop()));
                }
                ops.pop();
            } else if (Character.isDigit(token)) {
                long num = 0;
                while (i < expression.length() && Character.isDigit(expression.charAt(i))) {
                    num = num * 10 + (expression.charAt(i) - '0');
                    i++;
                }
                i--;
                vals.push(num);
            }
        }
        while (!ops.isEmpty()) {
            vals.push(applyOp(ops.pop(), vals.pop(), vals.pop()));
        }
        return vals.pop();
    }

    private static long applyOp(char op, long b, long a) {
        if (op == '+') return a + b;
        if (op == '*') return a * b;
        throw new IllegalArgumentException("Unknown operator: " + op);
    }
}
