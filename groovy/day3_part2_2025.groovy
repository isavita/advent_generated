
import java.math.BigInteger

class Main {
    static void main(String[] args) {
        def total = BigInteger.ZERO
        new File('input.txt').eachLine { line ->
            line = line.trim()
            if (!line) return
            if (line.length() < 12) return
            def best = maxSubseq(line, 12)
            total = total.add(new BigInteger(best))
        }
        println "Total output joltage: $total"
    }

    private static String maxSubseq(String s, int k) {
        int n = s.length()
        int toRemove = n - k
        char[] stack = new char[n]
        int top = 0
        for (int i = 0; i < n; i++) {
            char c = s.charAt(i)
            while (toRemove > 0 && top > 0 && stack[top - 1] < c) {
                top--
                toRemove--
            }
            stack[top++] = c
        }
        return new String(stack, 0, k)
    }
}
