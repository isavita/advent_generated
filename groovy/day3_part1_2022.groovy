class Main {
    static int itemPriority(char c) {
        int code = c as int
        if (code >= 97 && code <= 122) {
            return code - 97 + 1
        } else {
            return code - 65 + 27
        }
    }

    static void main(String[] args) {
        int sum = 0
        def file = new File("input.txt")
        file.eachLine { line ->
            int half = line.length() / 2
            String first = line.substring(0, half)
            String second = line.substring(half)

            boolean[] seen = new boolean[128]
            for (int i = 0; i < first.length(); i++) {
                char ch = first.charAt(i)
                int code = ch as int
                if (code >= 0 && code < 128) seen[code] = true
            }

            boolean found = false
            for (int i = 0; i < second.length(); i++) {
                char ch = second.charAt(i)
                int code = ch as int
                if (!found && code >= 0 && code < 128 && seen[code]) {
                    sum += itemPriority(ch)
                    found = true
                }
            }
        }
        println(sum)
    }
}
Main.main(null)