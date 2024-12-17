
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

class Main {

    enum Mode {
        POSITION, IMMEDIATE, RELATIVE
    }

    enum Opcode {
        ADD, MUL, INPUT, OUTPUT, JT, JF, LT, EQ, RBO, HALT
    }

    static class DecodedInstruction {
        Opcode opcode;
        Mode[] modes;

        DecodedInstruction(Opcode opcode, Mode[] modes) {
            this.opcode = opcode;
            this.modes = modes;
        }
    }

    static DecodedInstruction decode(int n) {
        Opcode opcode = Opcode.values()[n % 100 - 1];
        if (opcode == Opcode.HALT) return new DecodedInstruction(opcode, null);
        n /= 100;
        Mode[] modes = new Mode[3];
        for (int i = 0; i < 3; i++) {
            modes[i] = Mode.values()[n % 10];
            n /= 10;
        }
        return new DecodedInstruction(opcode, modes);
    }

    static class Machine {
        Map<Integer, Integer> data;
        int ip;
        Queue<Integer> in;
        Queue<Integer> out;
        int relbase;

        Machine(List<Integer> program, Queue<Integer> in, Queue<Integer> out) {
            this.data = new HashMap<>();
            for (int i = 0; i < program.size(); i++) {
                this.data.put(i, program.get(i));
            }
            this.in = in;
            this.out = out;
            this.ip = 0;
            this.relbase = 0;
        }

        int get(int i, Mode mo) {
            int val = data.getOrDefault(i, 0);
            switch (mo) {
                case IMMEDIATE:
                    return val;
                case POSITION:
                    return data.getOrDefault(val, 0);
                case RELATIVE:
                    return data.getOrDefault(relbase + val, 0);
                default:
                    throw new IllegalArgumentException("Unknown mode: " + mo);
            }
        }

        void set(int i, Mode mo, int val) {
            int pos = data.getOrDefault(i, 0);
            switch (mo) {
                case POSITION:
                    data.put(pos, val);
                    break;
                case RELATIVE:
                    data.put(relbase + pos, val);
                    break;
                default:
                    throw new IllegalArgumentException("Unknown mode: " + mo);
            }
        }

        boolean step() {
            DecodedInstruction instruction = decode(data.getOrDefault(ip, 0));
            Opcode op = instruction.opcode;
            if (op == Opcode.HALT) return false;
            Mode[] modes = instruction.modes;
            switch (op) {
                case ADD: {
                    int val = get(ip + 1, modes[0]) + get(ip + 2, modes[1]);
                    set(ip + 3, modes[2], val);
                    ip += 4;
                    break;
                }
                case MUL: {
                    int val = get(ip + 1, modes[0]) * get(ip + 2, modes[1]);
                    set(ip + 3, modes[2], val);
                    ip += 4;
                    break;
                }
                case INPUT: {
                    set(ip + 1, modes[0], in.poll());
                    ip += 2;
                    break;
                }
                case OUTPUT: {
                    out.offer(get(ip + 1, modes[0]));
                    ip += 2;
                    break;
                }
                case JT: {
                    if (get(ip + 1, modes[0]) != 0) {
                        ip = get(ip + 2, modes[1]);
                    } else {
                        ip += 3;
                    }
                    break;
                }
                case JF: {
                    if (get(ip + 1, modes[0]) == 0) {
                        ip = get(ip + 2, modes[1]);
                    } else {
                        ip += 3;
                    }
                    break;
                }
                case LT: {
                    if (get(ip + 1, modes[0]) < get(ip + 2, modes[1])) {
                        set(ip + 3, modes[2], 1);
                    } else {
                        set(ip + 3, modes[2], 0);
                    }
                    ip += 4;
                    break;
                }
                case EQ: {
                    if (get(ip + 1, modes[0]) == get(ip + 2, modes[1])) {
                        set(ip + 3, modes[2], 1);
                    } else {
                        set(ip + 3, modes[2], 0);
                    }
                    ip += 4;
                    break;
                }
                case RBO: {
                    relbase += get(ip + 1, modes[0]);
                    ip += 2;
                    break;
                }
                default:
                    throw new IllegalArgumentException("Unknown opcode: " + op);
            }
            return true;
        }

        void run() {
            while (step());
        }
    }

    enum Dir {
        N, E, S, W
    }

    static class Point {
        int x, y;

        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Point point = (Point) o;
            return x == point.x && y == point.y;
        }

        @Override
        public int hashCode() {
            return Objects.hash(x, y);
        }

        Point add(Point other) {
            return new Point(x + other.x, y + other.y);
        }
    }

    static final Map<Dir, Point> pointMap = Map.of(
            Dir.N, new Point(0, 1),
            Dir.E, new Point(1, 0),
            Dir.S, new Point(0, -1),
            Dir.W, new Point(-1, 0)
    );

    static final Point[] neighbors4 = {new Point(0, 1), new Point(0, -1), new Point(1, 0), new Point(-1, 0)};

    static int manhattan(Point p, Point q) {
        return Math.abs(p.x - q.x) + Math.abs(p.y - q.y);
    }

    static class Item<T> implements Comparable<Item<T>> {
        T obj;
        int priority;

        Item(T obj, int priority) {
            this.obj = obj;
            this.priority = priority;
        }

        @Override
        public int compareTo(Item<T> other) {
            return Integer.compare(other.priority, this.priority);
        }
    }

    static class PathFinder {
        Machine m;
        Map<Point, Character> grid;
        Queue<Integer> in;
        Queue<Integer> out;
        Map<Dir, Integer> dirmap;
        Point p;
        Point oxygen;

        PathFinder(List<Integer> program) {
            this.grid = new HashMap<>();
            this.in = new LinkedList<>();
            this.out = new LinkedList<>();
            this.dirmap = Map.of(Dir.N, 1, Dir.S, 2, Dir.W, 3, Dir.E, 4);
            this.p = new Point(0, 0);
            this.grid.put(p, '.');
            this.m = new Machine(program, in, out);
            new Thread(m::run).start();
        }

        boolean tryMove(Dir dir) {
            in.offer(dirmap.get(dir));
            Point next = p.add(pointMap.get(dir));
            int status = out.poll();
            switch (status) {
                case 0:
                    grid.put(next, '#');
                    return false;
                case 1:
                    grid.put(next, '.');
                    break;
                case 2:
                    grid.put(next, 'O');
                    oxygen = next;
                    break;
            }
            p = next;
            return true;
        }

        void explore() {
            while (!open().isEmpty()) {
                if (!open().containsKey(p)) {
                    int min = Integer.MAX_VALUE;
                    Point next = null;
                    for (Point to : open().keySet()) {
                        int dist = manhattan(p, to);
                        if (dist < min) {
                            min = dist;
                            next = to;
                        }
                    }
                    List<Dir> minpath = shortestPath(p, next);
                    for (Dir m : minpath) {
                        if (!tryMove(m)) {
                            throw new IllegalStateException("bad path");
                        }
                    }
                }
                while (true) {
                    Dir d = null;
                    for (Point n : neighbors4) {
                        if (!grid.containsKey(p.add(n))) {
                            d = dirFromPoint(n);
                            break;
                        }
                    }
                    if (d == null || !tryMove(d)) {
                        break;
                    }
                }
            }
        }

        static final Map<Point, Dir> fromPointMap = Map.of(
                new Point(0, 1), Dir.N,
                new Point(1, 0), Dir.E,
                new Point(0, -1), Dir.S,
                new Point(-1, 0), Dir.W
        );

        Dir dirFromPoint(Point p) {
            return fromPointMap.get(p);
        }

        Map<Point, Void> open() {
            Map<Point, Void> ps = new HashMap<>();
            for (Map.Entry<Point, Character> entry : grid.entrySet()) {
                Point p = entry.getKey();
                if (entry.getValue() == '#') {
                    continue;
                }
                for (Point n : neighbors4) {
                    if (!grid.containsKey(p.add(n))) {
                        ps.put(p, null);
                        break;
                    }
                }
            }
            return ps;
        }

        List<Dir> shortestPath(Point from, Point to) {
            PriorityQueue<Item<Point>> pq = new PriorityQueue<>();
            pq.offer(new Item<>(from, 0));
            Map<Point, List<Dir>> pathmap = new HashMap<>();
            pathmap.put(from, new ArrayList<>());
            while (!pq.isEmpty()) {
                Point curr = pq.poll().obj;
                List<Dir> currpath = pathmap.get(curr);
                if (curr.equals(to)) {
                    break;
                }
                for (Point n : neighbors4) {
                    Point next = curr.add(n);
                    if (!grid.containsKey(next) || grid.get(next) == '#') {
                        continue;
                    }
                    if (!pathmap.containsKey(next) || pathmap.get(next).size() > 1 + currpath.size()) {
                        List<Dir> nextpath = new ArrayList<>(currpath);
                        nextpath.add(dirFromPoint(n));
                        pq.offer(new Item<>(next, -nextpath.size()));
                        pathmap.put(next, nextpath);
                    }
                }
            }
            List<Dir> path = pathmap.get(to);
            if (path == null) {
                throw new IllegalStateException("no path");
            }
            return path;
        }

        int longestPath(Point from) {
            PriorityQueue<Item<Point>> pq = new PriorityQueue<>();
            pq.offer(new Item<>(from, 0));
            Map<Point, Integer> distmap = new HashMap<>();
            distmap.put(from, 0);
            int max = 0;
            while (!pq.isEmpty()) {
                Point curr = pq.poll().obj;
                int currdist = distmap.get(curr);
                max = Math.max(max, currdist);
                for (Point n : neighbors4) {
                    Point next = curr.add(n);
                    if (!grid.containsKey(next) || grid.get(next) == '#') {
                        continue;
                    }
                    if (!distmap.containsKey(next) || distmap.get(next) > 1 + currdist) {
                        int nextdist = 1 + currdist;
                        pq.offer(new Item<>(next, -nextdist));
                        distmap.put(next, nextdist);
                    }
                }
            }
            return max;
        }
    }

    static String readAll(String filepath) throws IOException {
        return Files.lines(Paths.get(filepath)).collect(Collectors.joining()).trim();
    }

    static int atoi(String s) {
        return Integer.parseInt(s);
    }

    public static void main(String[] args) throws IOException {
        List<Integer> program = Arrays.stream(readAll("input.txt").split(",")).map(Main::atoi).collect(Collectors.toList());
        PathFinder pf = new PathFinder(program);
        pf.explore();
        System.out.println(pf.longestPath(pf.oxygen));
    }
}
