
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

public class Solution {

    static class P {
        int x, y;

        P(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            P p = (P) o;
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
            switch (direction) {
                case 'R':
                    return values()[(ordinal() + 1) % 4];
                case 'L':
                    return values()[(ordinal() - 1 + 4) % 4];
                default:
                    return this;
            }
        }

        int points() {
            return (ordinal() + 3) % 4;
        }
    }

    static class Movement {
        int steps;
        char rotate;
    }

    static Map<P, Boolean> map = new HashMap<>();
    static int size;
    static List<Movement> movements = new ArrayList<>();
    static P[] dirs = {
            new P(-1, 0),
            new P(0, 1),
            new P(1, 0),
            new P(0, -1)
    };

    static class Human {
        P curr;
        Dir facing;
    }

    public static void main(String[] args) throws Exception {
        parse();

        Human human = new Human();
        human.curr = new P(0, size);
        human.facing = Dir.E;

        for (Movement mov : movements) {
            human.facing = human.facing.rotate(mov.rotate);
            for (int i = 0; i < mov.steps; i++) {
                if (!walk(human)) {
                    break;
                }
            }
        }

        System.out.println(1000 * (human.curr.x + 1) + 4 * (human.curr.y + 1) + human.facing.points());
    }

    static void parse() throws Exception {
        File file = new File("input.txt");
        Scanner scanner = new Scanner(file);
        int r = 0;
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            if (line.isEmpty()) {
                break;
            }
            if (r == 0) {
                size = line.length() / 3;
            }
            for (int c = 0; c < line.length(); c++) {
                char ch = line.charAt(c);
                if (ch == ' ') continue;
                map.put(new P(r, c), ch == '#');
            }
            r++;
        }
        if(scanner.hasNextLine()){
            movements = parsePath(scanner.nextLine());
        }
        scanner.close();
    }

    static List<Movement> parsePath(String path) {
        List<Movement> movements = new ArrayList<>();
        int acc = 0;
        for (char ch : path.toCharArray()) {
            switch (ch) {
                case 'R':
                    Movement m1 = new Movement();
                    m1.steps = acc;
                    movements.add(m1);
                    acc = 0;
                    Movement m2 = new Movement();
                    m2.rotate = 'R';
                    movements.add(m2);
                    break;
                case 'L':
                    Movement m3 = new Movement();
                    m3.steps = acc;
                    movements.add(m3);
                    acc = 0;
                    Movement m4 = new Movement();
                    m4.rotate = 'L';
                    movements.add(m4);
                    break;
                default:
                    acc = 10 * acc + (ch - '0');
            }
        }
        Movement m5 = new Movement();
        m5.steps = acc;
        movements.add(m5);
        return movements;
    }

    static boolean walk(Human h) {
        P dir = dirs[h.facing.ordinal()];
        P next = new P(h.curr.x + dir.x, h.curr.y + dir.y);
        if (map.containsKey(next)) {
            if (map.get(next)) {
                return false;
            }
            h.curr = next;
            return true;
        }

        P oppDir = new P(-dir.x, -dir.y);
        while (true) {
            P lookAhead = new P(next.x + oppDir.x, next.y + oppDir.y);
            if (!map.containsKey(lookAhead)) {
                if (map.get(next)) {
                    return false;
                }
                h.curr = next;
                return true;
            }
            next = lookAhead;
        }
    }
}
