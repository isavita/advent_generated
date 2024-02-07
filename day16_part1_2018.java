
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {

    public static void main(String[] args) {
        String inputStr = "";
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                inputStr += line + "\n";
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        String[] lines = inputStr.split("\n");

        List<OP> opcodes = new ArrayList<OP>();
        opcodes.add(new OP("addr", '+', 'r', 'r'));
        opcodes.add(new OP("addi", '+', 'r', 'v'));
        opcodes.add(new OP("mulr", '*', 'r', 'r'));
        opcodes.add(new OP("muli", '*', 'r', 'v'));
        opcodes.add(new OP("banr", '&', 'r', 'r'));
        opcodes.add(new OP("bani", '&', 'r', 'v'));
        opcodes.add(new OP("borr", '|', 'r', 'r'));
        opcodes.add(new OP("bori", '|', 'r', 'v'));
        opcodes.add(new OP("setr", 'a', 'r', 'r'));
        opcodes.add(new OP("seti", 'a', 'v', 'r'));
        opcodes.add(new OP("gtir", '>', 'v', 'r'));
        opcodes.add(new OP("gtri", '>', 'r', 'v'));
        opcodes.add(new OP("gtrr", '>', 'r', 'r'));
        opcodes.add(new OP("eqir", '=', 'v', 'r'));
        opcodes.add(new OP("eqri", '=', 'r', 'v'));
        opcodes.add(new OP("eqir", '=', 'r', 'r'));

        int sum = 0;
        int lineCount = 0;
        while (lineCount < lines.length) {
            if (lines[lineCount].length() > 0 && lines[lineCount].charAt(0) == 'B') {
                String[] split = lines[lineCount].split("[^0-9]+");
                int[] registers = new int[] {
                    strToInt(split[1]),
                    strToInt(split[2]),
                    strToInt(split[3]),
                    strToInt(split[4])
                };

                split = lines[lineCount + 1].split("[^0-9]+");
                byte[] instruction = new byte[] {
                    (byte) strToInt(split[0]),
                    (byte) strToInt(split[1]),
                    (byte) strToInt(split[2]),
                    (byte) strToInt(split[3])
                };

                split = lines[lineCount + 2].split("[^0-9]+");
                int[] n = new int[] {
                    strToInt(split[1]),
                    strToInt(split[2]),
                    strToInt(split[3]),
                    strToInt(split[4])
                };

                int tempSum = testCode(registers, n, instruction, opcodes);

                if (tempSum >= 3) {
                    sum++;
                }

                lineCount = lineCount + 4;
            } else {
                break;
            }
        }

        System.out.println(sum);
    }

    static void remove(OP op, byte c) {
        int i = -1;
        for (int j = 0; j < op.matchCount.size(); j++) {
            if (c == op.matchCount.get(j)) {
                i = j;
            }
        }
        if (i != -1) {
            op.matchCount.remove(i);
        }
    }

    static void add(OP op, byte c) {
        for (byte v : op.matchCount) {
            if (v == c) {
                return;
            }
        }
        op.matchCount.add(c);
    }

    static int testCode(int[] registers, int[] n, byte[] instruction, List<OP> opcodes) {
        int sum = 0;
        for (OP opcode : opcodes) {
            if (match(n, runOp(opcode, registers, instruction))) {
                add(opcode, instruction[0]);
                sum++;
            }
        }
        return sum;
    }

    static boolean match(int[] r, int[] c) {
        if (r.length != c.length) {
            return false;
        }
        for (int i = 0; i < r.length; i++) {
            if (r[i] != c[i]) {
                return false;
            }
        }
        return true;
    }

    static int[] runOp(OP op, int[] registers, byte[] instruction) {
        int[] registerCP = new int[4];
        System.arraycopy(registers, 0, registerCP, 0, registers.length);
        int A, B;
        if (op.a == 'r') {
            A = registerCP[instruction[1]];
        } else {
            A = instruction[1];
        }
        if (op.b == 'r') {
            B = registerCP[instruction[2]];
        } else {
            B = instruction[2];
        }
        switch (op.action) {
            case '+':
                registerCP[instruction[3]] = A + B;
                break;
            case '*':
                registerCP[instruction[3]] = A * B;
                break;
            case '&':
                registerCP[instruction[3]] = A & B;
                break;
            case '|':
                registerCP[instruction[3]] = A | B;
                break;
            case 'a':
                registerCP[instruction[3]] = A;
                break;
            case '>':
                if (A > B) {
                    registerCP[instruction[3]] = 1;
                } else {
                    registerCP[instruction[3]] = 0;
                }
                break;
            case '=':
                if (A == B) {
                    registerCP[instruction[3]] = 1;
                } else {
                    registerCP[instruction[3]] = 0;
                }
                break;
            default:
                System.out.println("not valid instruction");
        }
        return registerCP;
    }

    static int strToInt(String s) {
        return Integer.parseInt(s);
    }

    static String[] regSplit(String text, String delimeter) {
        return text.split(delimeter);
    }
}

class OP {
    char a;
    char b;
    char action;
    String name;
    List<Byte> matchCount;

    public OP(String name, char action, char a, char b) {
        this.name = name;
        this.action = action;
        this.a = a;
        this.b = b;
        this.matchCount = new ArrayList<Byte>();
    }
}
