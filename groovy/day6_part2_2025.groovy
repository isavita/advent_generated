
import java.math.BigInteger

class Main {
    static void main(String[] args) {
        def lines = new File('input.txt').readLines()
        lines = lines.collect { it.endsWith('\r') ? it[0..-2] : it }
        if (lines.isEmpty()) {
            println 'Grand total: 0'
            return
        }
        int lineCount = lines.size()
        int maxW = lines.collect { it.length() }.max()

        def isSep = new boolean[maxW]
        (0..<maxW).each { x ->
            boolean allSpace = true
            lineCount.times { r ->
                if (x < lines[r].length() && !Character.isWhitespace(lines[r][x] as char)) {
                    allSpace = false
                    return
                }
            }
            isSep[x] = allSpace
        }

        BigInteger grandTotal = BigInteger.ZERO
        boolean inBlock = false
        int start = 0
        (0..maxW).each { x ->
            boolean sep = (x == maxW) ? true : isSep[x]
            if (!sep) {
                if (!inBlock) { inBlock = true; start = x }
            } else if (inBlock) {
                grandTotal = grandTotal.add(processBlock(lines, lineCount, start, x - 1))
                inBlock = false
            }
        }

        println "Grand total: ${grandTotal}"
    }

    private static BigInteger processBlock(List<String> lines, int lineCount, int start, int end) {
        List<String> nums = []
        char op = '+'
        (start..end).each { c ->
            StringBuilder sb = new StringBuilder()
            lineCount.times { r ->
                if (c < lines[r].length()) {
                    char ch = lines[r][c] as char
                    if (ch.isDigit()) sb.append(ch)
                    else if (ch == '+' || ch == '*') op = ch
                }
            }
            if (sb.length()) nums << sb.toString()
        }
        if (nums.isEmpty()) return BigInteger.ZERO
        if (op == '*') {
            nums.collect { new BigInteger(it) }.inject(BigInteger.ONE) { a, b -> a.multiply(b) }
        } else {
            nums.collect { new BigInteger(it) }.inject(BigInteger.ZERO) { a, b -> a.add(b) }
        }
    }
}
