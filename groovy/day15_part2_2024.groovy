class Main {
  static class Coord {
    int x
    int y
    Coord(int x, int y) { this.x = x; this.y = y }
    Coord add(Coord o) { new Coord(this.x + o.x, this.y + o.y) }
    String key() { "${x},${y}" }
  }

  static Map<String,String> deepcopy_grid(Map<String,String> orig) {
    def copy = new HashMap<String,String>()
    orig.each { k, v -> copy[k] = v }
    return copy
  }

  static String get_grid(Map<String,String> grid, Coord c) { grid[c.key()] }

  static void set_grid(Map<String,String> grid, Coord c, String value) { grid[c.key()] = value }

  static boolean try_to_step(Map<String,String> m, Coord target_pos, Coord dir) {
    def orig_m = deepcopy_grid(m)
    boolean success = false
    def keyTarget = target_pos.key()
    def char_at_target = m.get(keyTarget)
    if (char_at_target == '.') {
      success = true
    } else if (char_at_target == 'O' || char_at_target == '@') {
      def pushed = target_pos.add(dir)
      if (try_to_step(m, pushed, dir)) {
        set_grid(m, pushed, char_at_target)
        set_grid(m, target_pos, '.')
        success = true
      }
    } else if (char_at_target == ']') {
      def pos_left_of_bracket = target_pos.add(new Coord(-1, 0))
      if (try_to_step(m, pos_left_of_bracket, dir)) {
        success = true
      }
    } else if (char_at_target == '[') {
      def pos_right_of_bracket = target_pos.add(new Coord(1, 0))
      if (dir.x == -1 && dir.y == 0) {
        def new_bracket_pos = target_pos.add(dir)
        if (try_to_step(m, new_bracket_pos, dir)) {
          set_grid(m, new_bracket_pos, '[')
          set_grid(m, target_pos, ']')
          set_grid(m, pos_right_of_bracket, '.')
          success = true
        }
      } else if (dir.x == 1 && dir.y == 0) {
        def new_bracket_pos = target_pos.add(new Coord(2, 0))
        if (try_to_step(m, new_bracket_pos, dir)) {
          set_grid(m, target_pos, '.')
          set_grid(m, pos_right_of_bracket, '[')
          set_grid(m, new_bracket_pos, ']')
          success = true
        }
      } else {
        def new_bracket_pos = target_pos.add(dir)
        def new_end_bracket_pos = pos_right_of_bracket.add(dir)
        if (try_to_step(m, new_bracket_pos, dir) && try_to_step(m, new_end_bracket_pos, dir)) {
          set_grid(m, target_pos, '.')
          set_grid(m, pos_right_of_bracket, '.')
          set_grid(m, new_bracket_pos, '[')
          set_grid(m, new_end_bracket_pos, ']')
          success = true
        }
      }
    }

    if (!success) {
      m.clear()
      m.putAll(orig_m)
    }
    return success
  }

  static long solve(String input_str) {
    def blocks = []
    def rest = input_str
    while (true) {
      int j = rest.indexOf("\n\n")
      if (j == -1) { blocks << rest; break }
      blocks << rest.substring(0, j)
      rest = rest.substring(j + 2)
    }
    def grid_str = blocks[0]
    def step_str = blocks[1].replace("\n","")

    def m = new HashMap<String,String>()
    Coord robot_pos = null

    def lines = grid_str.split(/\r?\n/)
    for (int y = 0; y < lines.length; y++) {
      def row = lines[y]
      for (int x = 0; x < row.length(); x++) {
        def ch = row.charAt(x)
        def p = new Coord(x, y)
        m[p.key()] = ch as String
        if (ch == '@') robot_pos = p
      }
    }

    def steps = new ArrayList<Coord>()
    for (int i = 0; i < step_str.length(); i++) {
      def ch = step_str.charAt(i)
      if (ch == '^') steps.add(new Coord(0, -1))
      else if (ch == '<') steps.add(new Coord(-1, 0))
      else if (ch == '>') steps.add(new Coord(1, 0))
      else if (ch == 'v') steps.add(new Coord(0, 1))
    }

    for (def dir : steps) {
      def target_pos = robot_pos.add(dir)
      if (try_to_step(m, target_pos, dir)) {
        def robot_char = get_grid(m, robot_pos)
        set_grid(m, target_pos, robot_char)
        set_grid(m, robot_pos, '.')
        robot_pos = target_pos
      }
    }

    long score = 0
    m.each { k, v ->
      if (v == '[' || v == 'O') {
        def parts = k.split(",")
        int kx = parts[0].toInteger()
        int ky = parts[1].toInteger()
        score += kx + 100 * ky
      }
    }
    return score
  }

  static String scaleUp(String input_str) {
    String s = input_str.replace("#","##")
    s = s.replace(".","..")
    s = s.replace("O","[]")
    s = s.replace("@","@.")
    return s
  }

  static void main(String[] args) {
    def file = new File("input.txt")
    if (!file.exists()) {
      System.err.println("Error: Could not open input.txt")
      return
    }
    String input_str = file.text
    long score1 = solve(input_str)
    println(score1)
    String scaled_input_str = scaleUp(input_str)
    long score2 = solve(scaled_input_str)
    println(score2)
  }
}

Main.main(null)