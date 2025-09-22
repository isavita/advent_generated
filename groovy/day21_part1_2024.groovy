class Main {
  static final List<String> KEY_PAD = ["789","456","123"," 0A"]
  static final List<String> ROBOT_PAD = [" ^A","<v>"]
  static final int MAX_ROBOTS = 3
  static Map<String, Long> memo = [:]

  static int[] findPosition(List<String> mat, char ch) {
    for (int i = 0; i < mat.size(); i++) {
      String row = mat[i]
      for (int j = 0; j < row.length(); j++) {
        if (row.charAt(j) == ch) return [i, j] as int[]
      }
    }
    return [-1, -1] as int[]
  }

  static boolean ok(List<String> mat, int[] st, String seq) {
    int currI = st[0]
    int currJ = st[1]
    int maxRows = mat.size()
    if (maxRows == 0) return false
    int maxCols = mat[0].length()
    if (currI < 0 || currI >= maxRows || currJ < 0 || currJ >= maxCols || mat[currI].charAt(currJ) == ' ') return false

    for (int idx = 0; idx < seq.length(); idx++) {
      char mv = seq.charAt(idx)
      if (mv == '^') currI--
      else if (mv == 'v') currI++
      else if (mv == '<') currJ--
      else if (mv == '>') currJ++
      if (currI < 0 || currI >= maxRows || currJ < 0 || currJ >= maxCols || mat[currI].charAt(currJ) == ' ') return false
    }
    return true
  }

  static String generateMoves(int[] position, char objective, List<String> pad) {
    int[] objPos = findPosition(pad, objective)
    int posI = position[0]
    int posJ = position[1]
    int objI = objPos[0]
    int objJ = objPos[1]

    String ret1 = ""
    if (posJ > objJ) ret1 += '<' * (posJ - objJ)
    if (posI > objI) ret1 += '^' * (posI - objI)
    if (posI < objI) ret1 += 'v' * (objI - posI)
    if (posJ < objJ) ret1 += '>' * (objJ - posJ)

    if (ok(pad, position, ret1)) {
      return ret1
    }

    String ret2 = ""
    if (posJ < objJ) ret2 += '>' * (objJ - posJ)
    if (posI > objI) ret2 += '^' * (posI - objI)
    if (posI < objI) ret2 += 'v' * (objI - posI)
    if (posJ > objJ) ret2 += '<' * (posJ - objJ)

    if (ok(pad, position, ret2)) {
      return ret2
    }
    return ""
  }

  static long solve(String code, int robots) {
    if (robots <= 0) {
      return code.length()
    }

    String key = code + "|" + robots
    if (memo.containsKey(key)) {
      return memo[key]
    }

    long ret = 0L
    int[] currentPos
    List<String> currentPad

    if (robots == MAX_ROBOTS) {
      currentPos = [3, 2] as int[]
      currentPad = KEY_PAD
    } else {
      currentPos = [0, 2] as int[]
      currentPad = ROBOT_PAD
    }

    for (int idx = 0; idx < code.length(); idx++) {
      char ch = code.charAt(idx)
      String moves = generateMoves(currentPos, ch, currentPad)
      int[] objPos = findPosition(currentPad, ch)
      currentPos = objPos
      ret += solve(moves + "A", robots - 1)
    }

    memo[key] = ret
    return ret
  }

  static void main(String[] args) {
    def input = new File("input.txt")
    if (!input.exists()) {
      System.exit(1)
    }

    long total = 0L
    input.eachLine { line ->
      String s = line.trim()
      if (s.isEmpty()) {
        return
      }
      def digits = s.findAll(/\d/).join('')
      long numericPart = digits ? digits.toLong() : 0L
      memo.clear()
      long sv = solve(s, MAX_ROBOTS)
      total += sv * numericPart
    }

    println(total)
  }
}
Main.main(null)