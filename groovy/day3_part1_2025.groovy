
import java.io.File

class Main {
    static void main(String[] args) {
        long total = 0
        File file = new File("input.txt")
        if (file.exists()) {
            file.eachLine { line ->
                def s = line.trim()
                if (s) total += calculateMaxJoltage(s)
            }
        }
        println "Total output joltage: $total"
    }

    static int calculateMaxJoltage(String bank) {
        for (int d1 = 9; d1 >= 0; d1--) {
            int idx = bank.indexOf(d1.toString())
            if (idx != -1 && idx < bank.length() - 1) {
                int maxD2 = -1
                for (int i = idx + 1; i < bank.length(); i++) {
                    char c = bank.charAt(i)
                    if (Character.isDigit(c)) {
                        int v = c - (char)'0'
                        if (v > maxD2) {
                            maxD2 = v
                            if (maxD2 == 9) break
                        }
                    }
                }
                if (maxD2 != -1) return d1 * 10 + maxD2
            }
        }
        0
    }
}
