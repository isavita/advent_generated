
import java.io.File;
import java.io.IOException;
import java.util.*;

public class Intcode {

    static class Machine {
        Map<Integer, Long> data;
        int ip;
        int relbase;
        Iterator<Long> inStream;
        List<Long> outStream;

        Machine(long[] program, Iterator<Long> inStream, List<Long> outStream) {
            this.data = new HashMap<>();
            for (int i = 0; i < program.length; i++) {
                this.data.put(i, program[i]);
            }
            this.ip = 0;
            this.relbase = 0;
            this.inStream = inStream;
            this.outStream = outStream;
        }

        long get(int i, int mo) {
            if (mo == 0) {
                return this.data.getOrDefault(this.data.getOrDefault(i, 0L).intValue(), 0L);
            } else if (mo == 1) {
                return this.data.getOrDefault(i, 0L);
            } else {
                return this.data.getOrDefault(this.relbase + this.data.getOrDefault(i, 0L).intValue(), 0L);
            }
        }

        void set(int i, int mo, long val) {
            if (mo == 0) {
                this.data.put(this.data.get(i).intValue(), val);
            } else {
                this.data.put(this.relbase + this.data.get(i).intValue(), val);
            }
        }

        boolean step() {
            int[] decoded = decode(this.data.getOrDefault(this.ip, 0L).intValue());
            int op = decoded[0];
            int[] modes = Arrays.copyOfRange(decoded, 1, decoded.length);

            if (op == 1) {
                long val = this.get(this.ip + 1, modes[0]) + this.get(this.ip + 2, modes[1]);
                this.set(this.ip + 3, modes[2], val);
                this.ip += 4;
            } else if (op == 2) {
                long val = this.get(this.ip + 1, modes[0]) * this.get(this.ip + 2, modes[1]);
                this.set(this.ip + 3, modes[2], val);
                this.ip += 4;
            } else if (op == 3) {
                this.set(this.ip + 1, modes[0], this.inStream.next());
                this.ip += 2;
            } else if (op == 4) {
                this.outStream.add(this.get(this.ip + 1, modes[0]));
                this.ip += 2;
            } else if (op == 5) {
                if (this.get(this.ip + 1, modes[0]) != 0) {
                    this.ip = (int) this.get(this.ip + 2, modes[1]);
                } else {
                    this.ip += 3;
                }
            } else if (op == 6) {
                if (this.get(this.ip + 1, modes[0]) == 0) {
                    this.ip = (int) this.get(this.ip + 2, modes[1]);
                } else {
                    this.ip += 3;
                }
            } else if (op == 7) {
                this.set(this.ip + 3, modes[2], this.get(this.ip + 1, modes[0]) < this.get(this.ip + 2, modes[1]) ? 1 : 0);
                this.ip += 4;
            } else if (op == 8) {
                this.set(this.ip + 3, modes[2], this.get(this.ip + 1, modes[0]) == this.get(this.ip + 2, modes[1]) ? 1 : 0);
                this.ip += 4;
            } else if (op == 9) {
                this.relbase += (int) this.get(this.ip + 1, modes[0]);
                this.ip += 2;
            } else if (op == 99) {
                return false;
            }
            return true;
        }

        void run() {
            while (this.step()) ;
        }
    }

    static int[] decode(int n) {
        int op = n % 100;
        int[] modes = new int[3];
        modes[0] = (n / 100) % 10;
        modes[1] = (n / 1000) % 10;
        modes[2] = (n / 10000) % 10;
        return new int[]{op, modes[0], modes[1], modes[2]};
    }

    static List<Long> run(long[] program, Iterator<Long> inStream) {
        List<Long> outStream = new ArrayList<>();
        Machine machine = new Machine(program, inStream, outStream);
        machine.run();
        return outStream;
    }

    static Map<AbstractMap.SimpleEntry<Integer, Integer>, Character> parse(long[] program) {
        List<Long> out = run(program, Collections.emptyIterator());
        Map<AbstractMap.SimpleEntry<Integer, Integer>, Character> scaffolding = new HashMap<>();
        int x = 0, y = 0;
        for (long o : out) {
            char c = (char) o;
            if (c == '\n') {
                y++;
                x = 0;
            } else {
                if (c == '^' || c == 'v' || c == '<' || c == '>') {
                    scaffolding.put(new AbstractMap.SimpleEntry<>(x, y), '#');
                } else if (c == '#') {
                    scaffolding.put(new AbstractMap.SimpleEntry<>(x, y), '#');
                }
                x++;
            }
        }
        return scaffolding;
    }

    static int sumAlign(Map<AbstractMap.SimpleEntry<Integer, Integer>, Character> grid) {
        int sum = 0;
        for (AbstractMap.SimpleEntry<Integer, Integer> p : grid.keySet()) {
            int x = p.getKey();
            int y = p.getValue();
            if (grid.containsKey(new AbstractMap.SimpleEntry<>(x, y + 1)) &&
                    grid.containsKey(new AbstractMap.SimpleEntry<>(x, y - 1)) &&
                    grid.containsKey(new AbstractMap.SimpleEntry<>(x + 1, y)) &&
                    grid.containsKey(new AbstractMap.SimpleEntry<>(x - 1, y))) {
                sum += x * y;
            }
        }
        return sum;
    }

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(new File("input.txt"));
        String[] parts = scanner.nextLine().split(",");
        long[] program = Arrays.stream(parts).mapToLong(Long::parseLong).toArray();
        scanner.close();

        Map<AbstractMap.SimpleEntry<Integer, Integer>, Character> scaffolding = parse(program);
        System.out.println(sumAlign(scaffolding));
    }
}
