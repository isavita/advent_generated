
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class Main {

    static class VM {
        Map<Long, Long> code;
        long ip;
        BlockingQueue<Long> input;
        BlockingQueue<Long> output;
        long relativeBase;

        VM(String filename) {
            code = new HashMap<>();
            input = new LinkedBlockingQueue<>();
            output = new LinkedBlockingQueue<>();
            load(filename);
        }

        void load(String filename) {
            try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
                String line = reader.readLine();
                String[] parts = line.trim().split(",");
                for (int i = 0; i < parts.length; i++) {
                    code.put((long) i, Long.parseLong(parts[i]));
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
            ip = 0;
            relativeBase = 0;
        }

        void run() {
            int arity;
            while (true) {
                long instruction = code.getOrDefault(ip, 0L);
                int opcode = (int) (instruction % 100);
                
                switch (opcode) {
                    case 1:
                        arity = 3;
                        long[] params1 = getParamsAddresses(ip, instruction, arity);
                        code.put(params1[2], code.getOrDefault(params1[0], 0L) + code.getOrDefault(params1[1], 0L));
                        break;
                    case 2:
                        arity = 3;
                        long[] params2 = getParamsAddresses(ip, instruction, arity);
                        code.put(params2[2], code.getOrDefault(params2[0], 0L) * code.getOrDefault(params2[1], 0L));
                        break;
                    case 3:
                        arity = 1;
                        long[] params3 = getParamsAddresses(ip, instruction, arity);
                        try {
                            code.put(params3[0], input.take());
                        } catch (InterruptedException e) {
                            Thread.currentThread().interrupt();
                            return;
                        }
                        break;
                    case 4:
                        arity = 1;
                        long[] params4 = getParamsAddresses(ip, instruction, arity);
                        output.offer(code.getOrDefault(params4[0], 0L));
                        break;
                    case 5:
                        arity = 2;
                        long[] params5 = getParamsAddresses(ip, instruction, arity);
                        if (code.getOrDefault(params5[0], 0L) != 0) {
                            ip = code.getOrDefault(params5[1], 0L);
                            continue;
                        }
                        break;
                    case 6:
                        arity = 2;
                        long[] params6 = getParamsAddresses(ip, instruction, arity);
                        if (code.getOrDefault(params6[0], 0L) == 0) {
                            ip = code.getOrDefault(params6[1], 0L);
                            continue;
                        }
                        break;
                    case 7:
                        arity = 3;
                        long[] params7 = getParamsAddresses(ip, instruction, arity);
                        code.put(params7[2], code.getOrDefault(params7[0], 0L) < code.getOrDefault(params7[1], 0L) ? 1L : 0L);
                        break;
                    case 8:
                        arity = 3;
                        long[] params8 = getParamsAddresses(ip, instruction, arity);
                        code.put(params8[2], code.getOrDefault(params8[0], 0L).equals(code.getOrDefault(params8[1], 0L)) ? 1L : 0L);
                        break;
                    case 9:
                        arity = 1;
                        long[] params9 = getParamsAddresses(ip, instruction, arity);
                        relativeBase += code.getOrDefault(params9[0], 0L);
                        break;
                    case 99:
                        return;
                    default:
                        throw new IllegalArgumentException("Invalid opcode: " + opcode);
                }
                ip += arity + 1;
            }
        }

        long[] getParamsAddresses(long pos, long instruction, int arity) {
            int[] modes = getModes(instruction, arity);
            long[] addresses = new long[arity];
            for (int i = 0; i < arity; i++) {
                addresses[i] = getParamAddress(pos + i + 1, modes[i]);
            }
            return addresses;
        }

        long getParamAddress(long pos, int mode) {
            switch (mode) {
                case 0:
                    return code.getOrDefault(pos, 0L);
                case 1:
                    return pos;
                case 2:
                    return relativeBase + code.getOrDefault(pos, 0L);
                default:
                    throw new IllegalArgumentException("Invalid mode: " + mode);
            }
        }

        int[] getModes(long instruction, int arity) {
            int modeSection = (int) (instruction / 100);
            int[] modes = new int[arity];
            for (int i = 0; i < arity; i++) {
                modes[i] = (modeSection / (int) Math.pow(10, i)) % 10;
            }
            return modes;
        }
    }

    static boolean beam(int x, int y) {
        VM vm = new VM("input.txt");
        new Thread(() -> vm.run()).start();
        vm.input.offer((long) x);
        vm.input.offer((long) y);
        try {
            return vm.output.take() == 1;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return false;
        }
    }

    public static void main(String[] args) {
        int y = 20;
        int x = 0;

        while (true) {
            if (!beam(x, y)) {
                x++;
                continue;
            }

            if (!beam(x + 99, y)) {
                y++;
                continue;
            }

            if (!beam(x, y + 99)) {
                x++;
                continue;
            }

            System.out.println(x * 10000 + y);
            return;
        }
    }
}
