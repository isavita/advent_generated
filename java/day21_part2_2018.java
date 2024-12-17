
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Paths.get("input.txt"));
        System.out.println(solve(input));
    }

    static int solve(String input) {
        OpcodeComputer opcodeComputer = parseInput(input);
        int lastReg5 = 0;
        Map<Integer, Boolean> comparedRegister5s = new HashMap<>();
        while (true) {
            if (opcodeComputer.registers[opcodeComputer.instructionPointer] == 28) {
                int reg5 = opcodeComputer.registers[5];
                if (comparedRegister5s.containsKey(reg5)) {
                    break;
                }
                comparedRegister5s.put(reg5, true);
                lastReg5 = reg5;
            }
            if(opcodeComputer.tick()) break;
        }
        return lastReg5;
    }

    static class OpcodeComputer {
        Instruction[] instructions;
        int[] registers = new int[6];
        int instructionPointer;

        boolean tick() {
            if (registers[instructionPointer] >= instructions.length) {
                return true;
            }
            Instruction inst = instructions[registers[instructionPointer]];
            registers = inst.opcodeFunc.apply(registers, inst.abcValues);
            registers[instructionPointer]++;
            return registers[instructionPointer] >= instructions.length;
        }
    }

    static class Instruction {
        OpcodeFunc opcodeFunc;
        int[] abcValues = new int[3];
    }

    interface OpcodeFunc {
        int[] apply(int[] registers, int[] abcValues);
    }

    static OpcodeComputer parseInput(String input) {
        String[] lines = input.split("\n");
        Pattern ipPattern = Pattern.compile("#ip (\\d+)");
        Matcher ipMatcher = ipPattern.matcher(lines[0]);
        int instructionPointer = 0;
        if (ipMatcher.find()) {
            instructionPointer = Integer.parseInt(ipMatcher.group(1));
        }

        List<Instruction> instructionsList = new java.util.ArrayList<>();
        Pattern instructionPattern = Pattern.compile("(\\w+) (\\d+) (\\d+) (\\d+)");
        for (int i = 1; i < lines.length; i++) {
            Matcher matcher = instructionPattern.matcher(lines[i]);
            if (matcher.find()) {
                Instruction inst = new Instruction();
                inst.opcodeFunc = opcodeNamesToFuncs.get(matcher.group(1));
                inst.abcValues[0] = Integer.parseInt(matcher.group(2));
                inst.abcValues[1] = Integer.parseInt(matcher.group(3));
                inst.abcValues[2] = Integer.parseInt(matcher.group(4));
                instructionsList.add(inst);
            }
        }

        OpcodeComputer computer = new OpcodeComputer();
        computer.instructions = instructionsList.toArray(new Instruction[0]);
        computer.instructionPointer = instructionPointer;
        return computer;
    }

    static final Map<String, OpcodeFunc> opcodeNamesToFuncs = new HashMap<>();

    static {
        opcodeNamesToFuncs.put("addr", (registers, abcValues) -> {
            registers[abcValues[2]] = registers[abcValues[0]] + registers[abcValues[1]];
            return registers;
        });
        opcodeNamesToFuncs.put("addi", (registers, abcValues) -> {
            registers[abcValues[2]] = registers[abcValues[0]] + abcValues[1];
            return registers;
        });
        opcodeNamesToFuncs.put("mulr", (registers, abcValues) -> {
            registers[abcValues[2]] = registers[abcValues[0]] * registers[abcValues[1]];
            return registers;
        });
        opcodeNamesToFuncs.put("muli", (registers, abcValues) -> {
            registers[abcValues[2]] = registers[abcValues[0]] * abcValues[1];
            return registers;
        });
        opcodeNamesToFuncs.put("banr", (registers, abcValues) -> {
            registers[abcValues[2]] = registers[abcValues[0]] & registers[abcValues[1]];
            return registers;
        });
        opcodeNamesToFuncs.put("bani", (registers, abcValues) -> {
            registers[abcValues[2]] = registers[abcValues[0]] & abcValues[1];
            return registers;
        });
        opcodeNamesToFuncs.put("borr", (registers, abcValues) -> {
            registers[abcValues[2]] = registers[abcValues[0]] | registers[abcValues[1]];
            return registers;
        });
        opcodeNamesToFuncs.put("bori", (registers, abcValues) -> {
            registers[abcValues[2]] = registers[abcValues[0]] | abcValues[1];
            return registers;
        });
        opcodeNamesToFuncs.put("setr", (registers, abcValues) -> {
            registers[abcValues[2]] = registers[abcValues[0]];
            return registers;
        });
        opcodeNamesToFuncs.put("seti", (registers, abcValues) -> {
            registers[abcValues[2]] = abcValues[0];
            return registers;
        });
        opcodeNamesToFuncs.put("gtir", (registers, abcValues) -> {
            registers[abcValues[2]] = abcValues[0] > registers[abcValues[1]] ? 1 : 0;
            return registers;
        });
        opcodeNamesToFuncs.put("gtri", (registers, abcValues) -> {
            registers[abcValues[2]] = registers[abcValues[0]] > abcValues[1] ? 1 : 0;
            return registers;
        });
        opcodeNamesToFuncs.put("gtrr", (registers, abcValues) -> {
            registers[abcValues[2]] = registers[abcValues[0]] > registers[abcValues[1]] ? 1 : 0;
            return registers;
        });
        opcodeNamesToFuncs.put("eqir", (registers, abcValues) -> {
            registers[abcValues[2]] = abcValues[0] == registers[abcValues[1]] ? 1 : 0;
            return registers;
        });
        opcodeNamesToFuncs.put("eqri", (registers, abcValues) -> {
            registers[abcValues[2]] = registers[abcValues[0]] == abcValues[1] ? 1 : 0;
            return registers;
        });
        opcodeNamesToFuncs.put("eqrr", (registers, abcValues) -> {
            registers[abcValues[2]] = registers[abcValues[0]] == registers[abcValues[1]] ? 1 : 0;
            return registers;
        });
    }
}
