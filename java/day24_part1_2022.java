
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class BlizzardBasin {

    private static final int[] DR = {0, 0, 1, -1, 0}; // Right, Left, Down, Up, Wait
    private static final int[] DC = {1, -1, 0, 0, 0};

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            List<String> lines = new ArrayList<>();
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
            reader.close();

            char[][] grid = new char[lines.size()][lines.get(0).length()];
            for (int i = 0; i < lines.size(); i++) {
                grid[i] = lines.get(i).toCharArray();
            }

            int startRow = 0;
            int startCol = -1;
            for (int i = 0; i < grid[0].length; i++) {
                if (grid[0][i] == '.') {
                    startCol = i;
                    break;
                }
            }

            int endRow = grid.length - 1;
            int endCol = -1;
            for (int i = 0; i < grid[endRow].length; i++) {
                if (grid[endRow][i] == '.') {
                    endCol = i;
                    break;
                }
            }
            
            int minutes = solve(grid, startRow, startCol, endRow, endCol);
            System.out.println(minutes);


        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        }
    }

    private static int solve(char[][] grid, int startRow, int startCol, int endRow, int endCol) {
        int rows = grid.length;
        int cols = grid[0].length;
        int lcm = lcm(rows - 2, cols - 2);

        Set<State> visited = new HashSet<>();
        Queue<State> queue = new LinkedList<>();
        State initialState = new State(startRow, startCol, 0);
        queue.offer(initialState);
        visited.add(initialState);

        while (!queue.isEmpty()) {
            State current = queue.poll();

            if (current.row == endRow && current.col == endCol) {
                return current.time;
            }

            for (int i = 0; i < 5; i++) {
                int newRow = current.row + DR[i];
                int newCol = current.col + DC[i];
                int newTime = current.time + 1;

                if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols && grid[newRow][newCol] != '#') {
                    if (!isBlizzard(grid, newRow, newCol, newTime, lcm)) {
                        State nextState = new State(newRow, newCol, newTime);
                        if (visited.add(nextState)) {
                          queue.offer(nextState);
                        }
                    }
                }
            }
        }
      return -1; // Should not reach here if there's a solution.
    }

    private static boolean isBlizzard(char[][] grid, int row, int col, int time, int lcm) {
      int rows = grid.length;
      int cols = grid[0].length;
      int r = rows - 2;
      int c = cols - 2;

        // Check for right-moving blizzards
        if (grid[row][(col - 1 - time % c + c * (Math.abs(time/c) + 1)) % c + 1] == '>') {
            return true;
        }
        // Check for left-moving blizzards
        if (grid[row][(col - 1 + time % c + c * (Math.abs(time/c) + 1)) % c + 1] == '<') {
            return true;
        }
        // Check for down-moving blizzards
        if (grid[(row - 1 - time % r + r * (Math.abs(time/r) + 1)) % r + 1][col] == 'v') {
            return true;
        }
        // Check for up-moving blizzards
        if (grid[(row - 1 + time % r + r * (Math.abs(time/r) + 1)) % r + 1][col] == '^') {
            return true;
        }

        return false;
    }


  private static int gcd(int a, int b) {
        while (b != 0) {
            int temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }

    private static int lcm(int a, int b) {
        return (a * b) / gcd(a, b);
    }


    private static class State {
        int row;
        int col;
        int time;

        public State(int row, int col, int time) {
            this.row = row;
            this.col = col;
            this.time = time;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            State state = (State) o;
            return row == state.row && col == state.col && time % lcm(state.row, state.col)== state.time % lcm(state.row, state.col);
        }
          
          @Override
            public int hashCode() {
                return Objects.hash(row, col, time%lcm(row,col));
            }
        private int lcm(int a, int b){
              if (a == 0 || b == 0) return 24*100; // return default value for non valid input, in this context, it can be considered as a "big" number
              return (a*b)/gcd(a,b);
        }

    }
}
