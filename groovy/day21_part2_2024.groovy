class Main {
    static Map<String, Long> memo = [:]

    static int[] findPosition(List<String> mat, char ch) {
        for (int i = 0; i < mat.size(); i++) {
            String line = mat.get(i)
            for (int j = 0; j < line.length(); j++) {
                if (line.charAt(j) == ch) return [i, j] as int[]
            }
        }
        return [-1, -1] as int[]
    }

    static boolean ok(List<String> mat, int[] st, String seq) {
        int currI = st[0]
        int currJ = st[1]
        for (int k = 0; k < seq.length(); k++) {
            char ch = seq.charAt(k)
            if (!(currI >= 0 && currI < mat.size() && currJ >= 0 && currJ < (mat.get(currI).length())) || mat.get(currI).charAt(currJ) == ' ') {
                return false
            }
            if (ch == '^') currI--
            else if (ch == 'v') currI++
            else if (ch == '<') currJ--
            else if (ch == '>') currJ++
        }
        return true
    }

    static String generateMoves(int[] position, char objective, List<String> pad) {
        int[] objPos = findPosition(pad, objective)
        int posI = position[0]
        int posJ = position[1]
        StringBuilder sb = new StringBuilder()
        int diff = posJ - objPos[1]
        if (diff > 0) for (int t = 0; t < diff; t++) sb.append('<')
        diff = posI - objPos[0]
        if (diff > 0) for (int t = 0; t < diff; t++) sb.append('^')
        diff = objPos[0] - posI
        if (diff > 0) for (int t = 0; t < diff; t++) sb.append('v')
        diff = objPos[1] - posJ
        if (diff > 0) for (int t = 0; t < diff; t++) sb.append('>')
        String res = sb.toString()
        if (!ok(pad, new int[]{posI, posJ}, res)) {
            StringBuilder alt = new StringBuilder()
            if (posJ < objPos[1]) for (int t = 0; t < objPos[1] - posJ; t++) alt.append('>')
            if (posI > objPos[0]) for (int t = 0; t < posI - objPos[0]; t++) alt.append('^')
            if (posI < objPos[0]) for (int t = 0; t < objPos[0] - posI; t++) alt.append('v')
            if (posJ > objPos[1]) for (int t = 0; t < posJ - objPos[1]; t++) alt.append('<')
            res = alt.toString()
        }
        return res
    }

    static long solve(String code, int robots, List<String> keyPad, List<String> robotPad, int maxRobots) {
        String key = code + ":" + robots + ":" + maxRobots
        if (memo.containsKey(key)) return memo.get(key)
        if (robots <= 0) {
            long v = code.length()
            memo.put(key, v)
            return v
        }

        long ret = 0L
        int posI = 3, posJ = 2
        if (robots != maxRobots) posI = 0

        for (int i = 0; i < code.length(); i++) {
            char ch = code.charAt(i)
            String moves
            if (robots == maxRobots) {
                moves = generateMoves(new int[]{posI, posJ}, ch, keyPad)
                int[] npos = findPosition(keyPad, ch)
                posI = npos[0]
                posJ = npos[1]
            } else {
                moves = generateMoves(new int[]{posI, posJ}, ch, robotPad)
                int[] npos = findPosition(robotPad, ch)
                posI = npos[0]
                posJ = npos[1]
            }
            ret += solve(moves + "A", robots - 1, keyPad, robotPad, maxRobots)
        }

        memo.put(key, ret)
        return ret
    }

    static void main(String[] args) {
        File inputFile = new File("input.txt")
        if (!inputFile.exists()) {
            println(0)
            return
        }
        String content = inputFile.text

        int maxRobots = 26
        List<String> keyPad = ["789", "456", "123", " 0A"]
        List<String> robotPad = [" ^A", "<v>"]

        long ret = 0L
        content.split(/\r?\n/).each { line ->
            String code = line.trim()
            if (code.length() > 0) {
                long numericPart = 0L
                for (int i = 0; i < code.length(); i++) {
                    char c = code.charAt(i)
                    if (c >= '0' && c <= '9') {
                        numericPart = numericPart * 10 + ((int) c - 48)
                    }
                }
                long val = solve(code, maxRobots, keyPad, robotPad, maxRobots)
                ret += val * numericPart
            }
        }
        println(ret)
    }
}
Main.main(null)