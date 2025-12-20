import java.math.BigInteger

class Main {
    static boolean isSep(List<String> lines, int col) {
        for (String line : lines) {
            if (col < line.length() && !Character.isWhitespace(line.charAt(col))) return false
        }
        return true
    }

    static void processBlock(List<String> lines, int sc, int ec, BigInteger[] grand) {
        List<BigInteger> nums = []
        int op = 0 // 0 none, 1 +, 2 *
        for (String line : lines) {
            int end = Math.min(ec + 1, line.length())
            if (sc >= line.length()) continue
            String seg = line.substring(sc, end).trim()
            if (seg.isEmpty()) continue
            if (seg == '+') op = 1
            else if (seg == '*') op = 2
            else nums << new BigInteger(seg)
        }
        if (nums.isEmpty()) return
        BigInteger acc
        if (op == 1) {
            acc = nums.inject(BigInteger.ZERO) { a, b -> a + b }
        } else if (op == 2) {
            acc = nums.inject(BigInteger.ONE) { a, b -> a * b }
        } else if (nums.size() == 1) {
            acc = nums[0]
        } else {
            acc = BigInteger.ZERO
        }
        grand[0] = grand[0] + acc
    }

    static void main(String[] args) {
        List<String> lines = new File('input.txt').readLines()
        int maxw = lines.collect { it.length() }.max() ?: 0
        BigInteger[] grand = [BigInteger.ZERO]
        boolean inb = false
        int sc = 0
        for (int x = 0; x < maxw; x++) {
            if (!isSep(lines, x)) {
                if (!inb) { inb = true; sc = x }
            } else {
                if (inb) { processBlock(lines, sc, x - 1, grand); inb = false }
            }
        }
        if (inb) processBlock(lines, sc, maxw - 1, grand)
        println "Grand total: ${grand[0]}"
    }
}