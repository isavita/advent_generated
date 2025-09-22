import java.util.Arrays

class Main {
    static int itemPriority(int code) {
        if (code >= 97 && code <= 122) return code - 97 + 1
        return code - 65 + 27
    }

    static void main(String[] args) {
        File file = new File("input.txt")
        if (!file.exists()) {
            println "Error opening file"
            System.exit(1)
        }

        def lines = file.readLines()
        def presence = [ new boolean[256], new boolean[256], new boolean[256] ]
        int groupLineCounter = 0
        int sum = 0

        for (String line : lines) {
            int len = line.length()
            for (int i = 0; i < len; i++) {
                char ch = line.charAt(i)
                if ((ch >= 'a' as char && ch <= 'z' as char) || (ch >= 'A' as char && ch <= 'Z' as char)) {
                    int idx = ch as int
                    presence[groupLineCounter][idx & 0xFF] = true
                }
            }

            groupLineCounter++
            if (groupLineCounter == 3) {
                int found = -1
                for (int i = 0; i < 256; i++) {
                    if (presence[0][i] && presence[1][i] && presence[2][i]) {
                        found = i
                        break
                    }
                }
                if (found != -1) sum += itemPriority(found)
                Arrays.fill(presence[0], false)
                Arrays.fill(presence[1], false)
                Arrays.fill(presence[2], false)
                groupLineCounter = 0
            }
        }

        println sum
    }
}