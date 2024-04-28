import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class Main {

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String input = "";
        String line;
        while ((line = br.readLine()) != null) {
            input += line + "\n";
        }
        br.close();
        System.out.println(solve(input));
    }

    public static int solve(String input) {
        OpcodeComputer opcodeComputer = parseInput(input);
        while (!opcodeComputer.tick()) {
            if (opcodeComputer.registers[opcodeComputer.instructionPointer] == 28) {
                break;
            }
        }
        return opcodeComputer.registers[5];
    }

    public static OpcodeComputer parseInput(String input) {
        String[] lines = input.split("\n");
        int instructionPointer = Integer.parseInt(lines[0].split(" ")[1]);
        OpcodeComputer opcodeComputer = new OpcodeComputer();
        opcodeComputer.instructionPointer = instructionPointer;
        opcodeComputer.registers = new int[6];
        opcodeComputer.instructions = new Instruction[lines.length - 1];
        for (int i = 1; i < lines.length; i++) {
            String[] parts = lines[i].split(" ");
            Instruction instruction = new Instruction(parts[0], Integer.parseInt(parts[1]), Integer.parseInt(parts[2]), Integer.parseInt(parts[3]));
            opcodeComputer.instructions[i - 1] = instruction;
        }
        return opcodeComputer;
    }

    public static class OpcodeComputer {
        Instruction[] instructions;
        int[] registers;
        int instructionPointer;

        public boolean tick() {
            if (registers[instructionPointer] >= instructions.length) {
                System.out.println("Out of range instruction, terminating...");
                return true;
            }
            Instruction inst = instructions[registers[instructionPointer]];
            opcodeFunc opcodeFunc = opcodeNamesToFuncs.get(inst.name);
            registers = opcodeFunc.apply(registers, inst.abcValues);
            registers[instructionPointer]++;
            if (registers[instructionPointer] >= instructions.length) {
                return true;
            }
            return false;
        }
    }

    public static class Instruction {
        String name;
        int[] abcValues;

        public Instruction(String name, int a, int b, int c) {
            this.name = name;
            this.abcValues = new int[]{a, b, c};
        }
    }

    public interface opcodeFunc {
        int[] apply(int[] registers, int[] abcValues);
    }

    public static opcodeFunc addr = (registers, abcValues) -> {
        registers[abcValues[2]] = registers[abcValues[0]] + registers[abcValues[1]];
        return registers;
    };
    public static opcodeFunc addi = (registers, abcValues) -> {
        registers[abcValues[2]] = registers[abcValues[0]] + abcValues[1];
        return registers;
    };
    public static opcodeFunc mulr = (registers, abcValues) -> {
        registers[abcValues[2]] = registers[abcValues[0]] * registers[abcValues[1]];
        return registers;
    };
    public static opcodeFunc muli = (registers, abcValues) -> {
        registers[abcValues[2]] = registers[abcValues[0]] * abcValues[1];
        return registers;
    };
    public static opcodeFunc banr = (registers, abcValues) -> {
        registers[abcValues[2]] = registers[abcValues[0]] & registers[abcValues[1]];
        return registers;
    };
    public static opcodeFunc bani = (registers, abcValues) -> {
        registers[abcValues[2]] = registers[abcValues[0]] & abcValues[1];
        return registers;
    };
    public static opcodeFunc borr = (registers, abcValues) -> {
        registers[abcValues[2]] = registers[abcValues[0]] | registers[abcValues[1]];
        return registers;
    };
    public static opcodeFunc bori = (registers, abcValues) -> {
        registers[abcValues[2]] = registers[abcValues[0]] | abcValues[1];
        return registers;
    };
    public static opcodeFunc setr = (registers, abcValues) -> {
        registers[abcValues[2]] = registers[abcValues[0]];
        return registers;
    };
    public static opcodeFunc seti = (registers, abcValues) -> {
        registers[abcValues[2]] = abcValues[0];
        return registers;
    };
    public static opcodeFunc gtir = (registers, abcValues) -> {
        registers[abcValues[2]] = abcValues[0] > registers[abcValues[1]] ? 1 : 0;
        return registers;
    };
    public static opcodeFunc gtri = (registers, abcValues) -> {
        registers[abcValues[2]] = registers[abcValues[0]] > abcValues[1] ? 1 : 0;
        return registers;
    };
    public static opcodeFunc gtrr = (registers, abcValues) -> {
        registers[abcValues[2]] = registers[abcValues[0]] > registers[abcValues[1]] ? 1 : 0;
        return registers;
    };
    public static opcodeFunc eqir = (registers, abcValues) -> {
        registers[abcValues[2]] = abcValues[0] == registers[abcValues[1]] ? 1 : 0;
        return registers;
    };
    public static opcodeFunc eqri = (registers, abcValues) -> {
        registers[abcValues[2]] = registers[abcValues[0]] == abcValues[1] ? 1 : 0;
        return registers;
    };
    public static opcodeFunc eqrr = (registers, abcValues) -> {
        registers[abcValues[2]] = registers[abcValues[0]] == registers[abcValues[1]] ? 1 : 0;
        return registers;
    };

    public static Map<String, opcodeFunc> opcodeNamesToFuncs = new HashMap<>();
    static {
        opcodeNamesToFuncs.put("addr", addr);
        opcodeNamesToFuncs.put("addi", addi);
        opcodeNamesToFuncs.put("mulr", mulr);
        opcodeNamesToFuncs.put("muli", muli);
        opcodeNamesToFuncs.put("banr", banr);
        opcodeNamesToFuncs.put("bani", bani);
        opcodeNamesToFuncs.put("borr", borr);
        opcodeNamesToFuncs.put("bori", bori);
        opcodeNamesToFuncs.put("setr", setr);
        opcodeNamesToFuncs.put("seti", seti);
        opcodeNamesToFuncs.put("gtir", gtir);
        opcodeNamesToFuncs.put("gtri", gtri);
        opcodeNamesToFuncs.put("gtrr", gtrr);
        opcodeNamesToFuncs.put("eqir", eqir);
        opcodeNamesToFuncs.put("eqri", eqri);
        opcodeNamesToFuncs.put("eqrr", eqrr);
    }
}