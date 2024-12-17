
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {

    interface Operation {
        int apply(int[] r, int a, int b);
    }

    static final Map<String, Operation> instructions = new HashMap<>();

    static {
        instructions.put("addr", (r, a, b) -> r[a] + r[b]);
        instructions.put("addi", (r, a, b) -> r[a] + b);
        instructions.put("mulr", (r, a, b) -> r[a] * r[b]);
        instructions.put("muli", (r, a, b) -> r[a] * b);
        instructions.put("banr", (r, a, b) -> r[a] & r[b]);
        instructions.put("bani", (r, a, b) -> r[a] & b);
        instructions.put("borr", (r, a, b) -> r[a] | r[b]);
        instructions.put("bori", (r, a, b) -> r[a] | b);
        instructions.put("setr", (r, a, b) -> r[a]);
        instructions.put("seti", (r, a, b) -> a);
        instructions.put("gtir", (r, a, b) -> a > r[b] ? 1 : 0);
        instructions.put("gtri", (r, a, b) -> r[a] > b ? 1 : 0);
        instructions.put("gtrr", (r, a, b) -> r[a] > r[b] ? 1 : 0);
        instructions.put("eqir", (r, a, b) -> a == r[b] ? 1 : 0);
        instructions.put("eqri", (r, a, b) -> r[a] == b ? 1 : 0);
        instructions.put("eqrr", (r, a, b) -> r[a] == r[b] ? 1 : 0);
    }

    public static void main(String[] args) throws FileNotFoundException {
        File file = new File("input.txt");
        Scanner scanner = new Scanner(file);
        List<String> lines = new ArrayList<>();
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            if (!line.isEmpty()) {
                lines.add(line);
            }
        }
        scanner.close();

        int ipRegister = -1;
        List<Instruction> program = new ArrayList<>();
        Pattern pattern = Pattern.compile("\\d+");

        for (String line : lines) {
            if (line.startsWith("#ip")) {
                ipRegister = Integer.parseInt(line.split(" ")[1]);
                continue;
            }

            String[] parts = line.split(" ");
            Operation op = instructions.get(parts[0]);
            Matcher matcher = pattern.matcher(line);
            List<Integer> nums = new ArrayList<>();
            while (matcher.find()) {
                nums.add(Integer.parseInt(matcher.group()));
            }
            int a = nums.get(0);
            int b = nums.get(1);
            int c = nums.get(2);

            program.add(new Instruction(op, a, b, c));
        }

        int[] registers = new int[6];
        registers[0] = 1;
        registers = runProgram(ipRegister, program, registers, 1000);
        int n = max(registers);
        int total = 0;
        for (int i = 1; i <= n; i++) {
            if (n % i == 0) {
                total += i;
            }
        }
        System.out.println(total);
    }

    static class Instruction {
        Operation op;
        int a, b, c;

        Instruction(Operation op, int a, int b, int c) {
            this.op = op;
            this.a = a;
            this.b = b;
            this.c = c;
        }
    }

    static int[] runProgram(int ipRegister, List<Instruction> program, int[] registers, int maxCycles) {
        int ip = 0;
        int cycles = 0;

        while (ip >= 0 && ip < program.size()) {
            registers[ipRegister] = ip;
            Instruction instruction = program.get(ip);
            registers[instruction.c] = instruction.op.apply(registers, instruction.a, instruction.b);
            ip = registers[ipRegister] + 1;
            cycles++;
            if (maxCycles > 0 && cycles >= maxCycles) {
                break;
            }
        }
        return registers;
    }

    static int max(int[] slice) {
        int maxValue = slice[0];
        for (int v : slice) {
            if (v > maxValue) {
                maxValue = v;
            }
        }
        return maxValue;
    }
}
