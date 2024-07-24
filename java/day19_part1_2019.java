
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

public class IntcodeVM {
    private final Map<Integer, Long> code;
    private int ip, relativeBase;
    private final BlockingQueue<Long> input = new ArrayBlockingQueue<>(1);
    private final BlockingQueue<Long> output = new ArrayBlockingQueue<>(1);

    public IntcodeVM(String filename) throws IOException {
        code = new HashMap<>();
        List<String> lines = Files.readAllLines(Paths.get(filename));
        String[] parts = lines.get(0).trim().split(",");
        for (int i = 0; i < parts.length; i++) {
            code.put(i, Long.parseLong(parts[i]));
        }
        ip = 0;
        relativeBase = 0;
    }

    public void run() {
        while (true) {
            long cmd = code.getOrDefault(ip, 0L);
            int opcode = (int) (cmd % 100);
            switch (opcode) {
                case 1: // add
                    code.put(getParamAddress(3), getParamValue(1) + getParamValue(2));
                    ip += 4;
                    break;
                case 2: // multiply
                    code.put(getParamAddress(3), getParamValue(1) * getParamValue(2));
                    ip += 4;
                    break;
                case 3: // read
                    code.put(getParamAddress(1), readInput());
                    ip += 2;
                    break;
                case 4: // write
                    writeOutput(getParamValue(1));
                    ip += 2;
                    break;
                case 5: // jump if true
                    ip = getParamValue(1) != 0 ? (int) getParamValue(2) : ip + 3;
                    break;
                case 6: // jump if false
                    ip = getParamValue(1) == 0 ? (int) getParamValue(2) : ip + 3;
                    break;
                case 7: // less than
                    code.put(getParamAddress(3), getParamValue(1) < getParamValue(2) ? 1L : 0L);
                    ip += 4;
                    break;
                case 8: // equals
                    code.put(getParamAddress(3), getParamValue(1) == getParamValue(2) ? 1L : 0L);
                    ip += 4;
                    break;
                case 9: // adjust relative base
                    relativeBase += getParamValue(1);
                    ip += 2;
                    break;
                case 99: // halt
                    return;
                default:
                    throw new IllegalArgumentException("Unknown opcode: " + opcode);
            }
        }
    }

    private long getParamValue(int offset) {
        return code.getOrDefault(getParamAddress(offset), 0L);
    }

    private int getParamAddress(int offset) {
        long cmd = code.getOrDefault(ip, 0L);
        int mode = (int) (cmd / Math.pow(10, offset + 1)) % 10;
        switch (mode) {
            case 0: return (int) code.getOrDefault(ip + offset, 0L).intValue(); // position mode
            case 1: return ip + offset; // immediate mode
            case 2: return relativeBase + (int) code.getOrDefault(ip + offset, 0L).intValue(); // relative mode
            default: throw new IllegalArgumentException("Unknown mode: " + mode);
        }
    }

    private long readInput() {
        try {
            return input.take();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return 0;
        }
    }

    private void writeOutput(long value) {
        output.offer(value);
    }

    public void setInput(long value) {
        try {
            input.put(value);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    public long getOutput() {
        try {
            return output.take();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return 0;
        }
    }

    public static void main(String[] args) throws IOException {
        int sum = 0;
        for (int y = 0; y < 50; y++) {
            for (int x = 0; x < 50; x++) {
                if (beam(x, y)) {
                    sum++;
                }
            }
        }
        System.out.println(sum);
    }

    private static boolean beam(int x, int y) throws IOException {
        IntcodeVM vm = new IntcodeVM("input.txt");
        new Thread(vm::run).start();
        vm.setInput(x);
        vm.setInput(y);
        return vm.getOutput() == 1;
    }
}
