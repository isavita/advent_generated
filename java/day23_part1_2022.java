
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

public class Main {

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
            int result = x;
            result = 31 * result + y;
            return result;
        }
    }

    static class Elf {
        P pos;
        boolean moving;
        P nextPos;

        Elf(P pos) {
            this.pos = pos;
        }
    }

    static final int N = 1;
    static final int E = 3;
    static final int S = 5;
    static final int W = 7;

    static Map<P, Boolean> map = new HashMap<>();
    static List<Elf> elves = new ArrayList<>();
    static final int[] order = {N, S, W, E};
    static int currDir = 0;
    static final P[] dirs = {
            new P(-1, -1),
            new P(-1, 0),
            new P(-1, +1),
            new P(0, +1),
            new P(+1, +1),
            new P(+1, 0),
            new P(+1, -1),
            new P(0, -1)
    };

    static boolean aroundAllEmpty(Elf e) {
        for (P d : dirs) {
            P adj = new P(e.pos.x + d.x, e.pos.y + d.y);
            if (map.containsKey(adj)) {
                return false;
            }
        }
        return true;
    }

    static boolean elfInDirection(Elf e, int wannaGo) {
        for (int j = -1; j <= 1; j++) {
            P dxy = dirs[(wannaGo + j + 8) % 8];
            P adj = new P(e.pos.x + dxy.x, e.pos.y + dxy.y);
            if (map.containsKey(adj)) {
                return true;
            }
        }
        return false;
    }

    static boolean run() {
        Map<P, Integer> proposes = new HashMap<>();

        for (Elf e : elves) {
            if (aroundAllEmpty(e)) {
                continue;
            }

            for (int i = 0; i < 4; i++) {
                int dir = order[(currDir + i) % 4];

                if (elfInDirection(e, dir)) {
                    continue;
                }

                P dxy = dirs[dir];
                P dest = new P(e.pos.x + dxy.x, e.pos.y + dxy.y);
                proposes.put(dest, proposes.getOrDefault(dest, 0) + 1);
                e.nextPos = dest;
                e.moving = true;
                break;
            }
        }

        boolean someoneMoved = false;
        for (Elf e : elves) {
            if (!e.moving) {
                continue;
            }

            if (proposes.getOrDefault(e.nextPos, 0) > 1) {
                e.moving = false;
                continue;
            }

            someoneMoved = true;
            map.remove(e.pos);
            map.put(e.nextPos, true);
            e.pos = e.nextPos;
            e.moving = false;
        }

        currDir = (currDir + 1) % 4;
        return someoneMoved;
    }

    static P[] minMax() {
        P min = new P(Integer.MAX_VALUE, Integer.MAX_VALUE);
        P max = new P(Integer.MIN_VALUE, Integer.MIN_VALUE);
        for (P p : map.keySet()) {
            min.x = Math.min(min.x, p.x);
            min.y = Math.min(min.y, p.y);
            max.x = Math.max(max.x, p.x);
            max.y = Math.max(max.y, p.y);
        }
        return new P[]{min, max};
    }

    static void parse() {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            int row = 0;
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                for (int col = 0; col < line.length(); col++) {
                    if (line.charAt(col) == '#') {
                        P p = new P(row, col);
                        map.put(p, true);
                        elves.add(new Elf(p));
                    }
                }
                row++;
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        parse();

        for (int i = 0; i < 10; i++) {
            run();
        }

        P[] mm = minMax();
        P min = mm[0];
        P max = mm[1];

        int count = 0;
        for (int x = min.x; x <= max.x; x++) {
            for (int y = min.y; y <= max.y; y++) {
                if (!map.containsKey(new P(x, y))) {
                    count++;
                }
            }
        }

        System.out.println(count);
    }
}
