import java.io.*;
import java.util.*;

public class Main {

    static class Machine {
        Map<Integer, Integer> data;
        int ip;
        int relbase;
        Queue<Integer> in;
        Queue<Integer> out;

        Machine(int[] program, Queue<Integer> in, Queue<Integer> out) {
            this.data = new HashMap<>();
            for (int i = 0; i < program.length; i++) {
                this.data.put(i, program[i]);
            }
            this.in = in;
            this.out = out;
        }

        int get(int i, int mode) {
            switch (mode) {
                case 0:
                    return data.getOrDefault(data.get(i), 0);
                case 1:
                    return data.getOrDefault(i, 0);
                case 2:
                    return data.getOrDefault(relbase + data.get(i), 0);
                default:
                    throw new RuntimeException("Unknown mode: " + mode);
            }
        }

        void set(int i, int mode, int val) {
            switch (mode) {
                case 0:
                    data.put(data.get(i), val);
                    break;
                case 2:
                    data.put(relbase + data.get(i), val);
                    break;
                default:
                    throw new RuntimeException("Unknown mode: " + mode);
            }
        }

        boolean step() {
            int op = data.get(ip);
            int[] modes = new int[3];
            for (int i = 0; i < 3; i++) {
                modes[i] = (op / (int) Math.pow(10, i + 2)) % 10;
            }
            op %= 100;

            switch (op) {
                case 1:
                    int val = get(ip + 1, modes[0]) + get(ip + 2, modes[1]);
                    set(ip + 3, modes[2], val);
                    ip += 4;
                    break;
                case 2:
                    val = get(ip + 1, modes[0]) * get(ip + 2, modes[1]);
                    set(ip + 3, modes[2], val);
                    ip += 4;
                    break;
                case 3:
                    int input = in.poll();
                    set(ip + 1, modes[0], input);
                    ip += 2;
                    break;
                case 4:
                    out.add(get(ip + 1, modes[0]));
                    ip += 2;
                    break;
                case 5:
                    if (get(ip + 1, modes[0]) != 0) {
                        ip = get(ip + 2, modes[1]);
                    } else {
                        ip += 3;
                    }
                    break;
                case 6:
                    if (get(ip + 1, modes[0]) == 0) {
                        ip = get(ip + 2, modes[1]);
                    } else {
                        ip += 3;
                    }
                    break;
                case 7:
                    if (get(ip + 1, modes[0]) < get(ip + 2, modes[1])) {
                        set(ip + 3, modes[2], 1);
                    } else {
                        set(ip + 3, modes[2], 0);
                    }
                    ip += 4;
                    break;
                case 8:
                    if (get(ip + 1, modes[0]) == get(ip + 2, modes[1])) {
                        set(ip + 3, modes[2], 1);
                    } else {
                        set(ip + 3, modes[2], 0);
                    }
                    ip += 4;
                    break;
                case 9:
                    relbase += get(ip + 1, modes[0]);
                    ip += 2;
                    break;
                case 99:
                    return false;
                default:
                    throw new RuntimeException("Unknown opcode: " + op);
            }
            return true;
        }

        void run() {
            while (step()) {
            }
        }
    }

    static int countBlocks(int[] program) {
        Map<String, Integer> grid = new HashMap<>();
        Queue<Integer> in = new LinkedList<>();
        Queue<Integer> out = new LinkedList<>();
        Machine machine = new Machine(program, in, out);
        machine.run();
        while (!out.isEmpty()) {
            int x = out.poll();
            int y = out.poll();
            grid.put(x + "," + y, out.poll());
        }
        int n = 0;
        for (int t : grid.values()) {
            if (t == 2) {
                n++;
            }
        }
        return n;
    }

    public static void main(String[] args) throws IOException {
        String input = readAll("input.txt");
        String[] parts = input.split(",");
        int[] program = new int[parts.length];
        for (int i = 0; i < parts.length; i++) {
            program[i] = Integer.parseInt(parts[i]);
        }
        System.out.println(countBlocks(program));
    }

    static String readAll(String filepath) throws IOException {
        File file = new File(filepath);
        FileInputStream fis = new FileInputStream(file);
        byte[] data = new byte[(int) file.length()];
        fis.read(data);
        fis.close();
        return new String(data, "UTF-8").trim();
    }
}