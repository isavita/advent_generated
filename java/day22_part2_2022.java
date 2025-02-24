
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Solution {

    static class P {
        int x, y;

        P(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) return true;
            if (obj == null || getClass() != obj.getClass()) return false;
            P p = (P) obj;
            return x == p.x && y == p.y;
        }

        @Override
        public int hashCode() {
            return 31 * x + y;
        }
    }

    enum Dir {
        N, E, S, W;

        Dir rotate(char direction) {
            if (direction == 'R') {
                return values()[(this.ordinal() + 1) % 4];
            } else if (direction == 'L') {
                return values()[(this.ordinal() - 1 + 4) % 4];
            }
            return this;
        }

        int points() {
            return (this.ordinal() + 3) % 4;
        }
    }

    static class Movement {
        int steps;
        Character rotate;

        Movement(int steps, Character rotate) {
            this.steps = steps;
            this.rotate = rotate;
        }
    }

    static class Human {
        P curr;
        Dir facing;

        Human(P curr, Dir facing) {
            this.curr = curr;
            this.facing = facing;
        }

        P[] walk(Map<P, Boolean> mapData, P[] dirs, int size) {
            P dirDelta = dirs[this.facing.ordinal()];
            P nextPos = new P(this.curr.x + dirDelta.x, this.curr.y + dirDelta.y);
            if (mapData.containsKey(nextPos)) {
                if (mapData.get(nextPos)) {
                    return new P[]{this.curr, new P(this.facing.ordinal(), 0)};
                } else {
                    return new P[]{nextPos, new P(this.facing.ordinal(), 0)};
                }
            } else {
                P[] crossed = crossBorder(nextPos, this.facing, size);
                if (mapData.getOrDefault(crossed[0], false)) {
                    return new P[]{this.curr, new P(this.facing.ordinal(), 0)};
                }
                return crossed;
            }
        }
    }

    static P[] crossBorder(P n, Dir dir, int size) {
        int x = n.x;
        int y = n.y;
        int S = size;

        if (x == -1 && y < 2 * S) {
            return new P[]{new P(y + 2 * S, x + 1), new P(Dir.E.ordinal(), 0)};
        } else if (x == -1 && y >= 2 * S) {
            return new P[]{new P(x + 4 * S, y - 2 * S), new P(Dir.N.ordinal(), 0)};
        } else if (x == S && dir == Dir.S) {
            return new P[]{new P(y - S, x + S - 1), new P(Dir.W.ordinal(), 0)};
        } else if (x == 2 * S - 1 && dir == Dir.N) {
            return new P[]{new P(y + S, x - S + 1), new P(Dir.E.ordinal(), 0)};
        } else if (x == 3 * S && dir == Dir.S) {
            return new P[]{new P(y + 2 * S, x - 2 * S - 1), new P(Dir.W.ordinal(), 0)};
        } else if (x == 4 * S) {
            return new P[]{new P(x - 4 * S, y + 2 * S), new P(Dir.S.ordinal(), 0)};
        } else if (y == -1 && x < 3 * S) {
            return new P[]{new P(3 * S - 1 - x, y + S + 1), new P(Dir.E.ordinal(), 0)};
        } else if (y == -1 && x >= 3 * S) {
            return new P[]{new P(y + 1, x - 2 * S), new P(Dir.S.ordinal(), 0)};
        } else if (y == S - 1 && x < S) {
            return new P[]{new P(3 * S - 1 - x, y - S + 1), new P(Dir.E.ordinal(), 0)};
        } else if (y == S - 1 && x >= S && dir == Dir.W) {
            return new P[]{new P(y + S + 1, x - S), new P(Dir.S.ordinal(), 0)};
        } else if (y == S && dir == Dir.E) {
            return new P[]{new P(y + 2 * S - 1, x - 2 * S), new P(Dir.N.ordinal(), 0)};
        } else if (y == 2 * S && x < 2 * S && dir == Dir.E) {
            return new P[]{new P(y - S - 1, x + S), new P(Dir.N.ordinal(), 0)};
        } else if (y == 2 * S && x >= 2 * S) {
            return new P[]{new P(3 * S - 1 - x, y + S - 1), new P(Dir.W.ordinal(), 0)};
        } else if (y == 3 * S) {
            return new P[]{new P(3 * S - 1 - x, y - S - 1), new P(Dir.W.ordinal(), 0)};
        } else {
            throw new RuntimeException("Not a border crossing");
        }
    }

    static List<Movement> parsePath(String path) {
        List<Movement> movements = new ArrayList<>();
        int acc = 0;
        for (char char_ : path.toCharArray()) {
            if (char_ == 'R' || char_ == 'L') {
                if (acc != 0) {
                    movements.add(new Movement(acc, null));
                    acc = 0;
                }
                movements.add(new Movement(0, char_));
            } else if (Character.isDigit(char_)) {
                acc = acc * 10 + (char_ - '0');
            }
        }
        if (acc != 0) {
            movements.add(new Movement(acc, null));
        }
        return movements;
    }

    static Object[] parseInput(String filename) throws IOException {
        Map<P, Boolean> mapData = new HashMap<>();
        int size = 0;
        List<Movement> movements = new ArrayList<>();

        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            int r = 0;
            while ((line = reader.readLine()) != null) {
                line = line.replaceAll("\\n", "");
                if (line.isEmpty()) {
                    break;
                }
                if (r == 0) {
                    size = line.length() / 3;
                }
                for (int c = 0; c < line.length(); c++) {
                    char char_ = line.charAt(c);
                    if (char_ == ' ') {
                        continue;
                    } else if (char_ == '#') {
                        mapData.put(new P(r, c), true);
                    } else if (char_ == '.') {
                        mapData.put(new P(r, c), false);
                    }
                }
                r++;
            }

            String movementLine = reader.readLine().trim();
            movements = parsePath(movementLine);
        }

        return new Object[]{mapData, size, movements};
    }

    public static void main(String[] args) throws IOException {
        Object[] parsed = parseInput("input.txt");
        Map<P, Boolean> mapData = (Map<P, Boolean>) parsed[0];
        int size = (int) parsed[1];
        List<Movement> movements = (List<Movement>) parsed[2];

        P[] dirs = {
                new P(-1, 0),
                new P(0, 1),
                new P(1, 0),
                new P(0, -1)
        };

        Human human = new Human(new P(0, size), Dir.E);

        for (Movement mov : movements) {
            if (mov.rotate != null) {
                human.facing = human.facing.rotate(mov.rotate);
            }
            for (int i = 0; i < mov.steps; i++) {
                P[] walked = human.walk(mapData, dirs, size);
                if (walked[0].equals(human.curr) && walked[1].x == human.facing.ordinal()) {
                    break;
                }
                human.curr = walked[0];
                human.facing = Dir.values()[walked[1].x];
            }
        }

        int finalValue = 1000 * (human.curr.x + 1) + 4 * (human.curr.y + 1) + human.facing.points();
        System.out.println(finalValue);
    }
}
