
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class solution {
    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String input = "";
        String line;
        while ((line = br.readLine()) != null) {
            input += line + "\n";
        }
        br.close();

        String[] lines = input.split("\n");

        OP[] opcodes = {
            new OP("addr", '+', 'r', 'r'),
            new OP("addi", '+', 'r', 'v'),
            new OP("mulr", '*', 'r', 'r'),
            new OP("muli", '*', 'r', 'v'),
            new OP("banr", '&', 'r', 'r'),
            new OP("bani", '&', 'r', 'v'),
            new OP("borr", '|', 'r', 'r'),
            new OP("bori", '|', 'r', 'v'),
            new OP("setr", 'a', 'r', 'r'),
            new OP("seti", 'a', 'v', 'r'),
            new OP("gtir", '>', 'v', 'r'),
            new OP("gtri", '>', 'r', 'v'),
            new OP("gtrr", '>', 'r', 'r'),
            new OP("eqir", '=', 'v', 'r'),
            new OP("eqri", '=', 'r', 'v'),
            new OP("eqir", '=', 'r', 'r')
        };

        int sum = 0;
        int lineCount = 0;
        while (lineCount < lines.length) {
            if (lines[lineCount].length() > 0 && lines[lineCount].charAt(0) == 'B') {
                String[] split = lines[lineCount].split("[^0-9]+");
                int[] registers = {Integer.parseInt(split[1]), Integer.parseInt(split[2]),
                        Integer.parseInt(split[3]), Integer.parseInt(split[4])};
                split = lines[lineCount + 1].split("[^0-9]+");
                byte[] instruction = {(byte) Integer.parseInt(split[0]), (byte) Integer.parseInt(split[1]),
                        (byte) Integer.parseInt(split[2]), (byte) Integer.parseInt(split[3])};
                split = lines[lineCount + 2].split("[^0-9]+");
                int[] result = {Integer.parseInt(split[1]), Integer.parseInt(split[2]),
                        Integer.parseInt(split[3]), Integer.parseInt(split[4])};
                int tempSum = testCode(registers, result, instruction, opcodes);

                if (tempSum >= 3) {
                    sum++;
                }

                lineCount = lineCount + 4;
            } else {
                break;
            }
        }

        Map<Byte, OP> orderedOpCodes = new HashMap<>();

        while (orderedOpCodes.size() < 16) {
            for (int i = 0; i < opcodes.length; i++) {
                if (opcodes[i].matchCount.length == 1) {
                    byte c = opcodes[i].matchCount[0];
                    orderedOpCodes.put(c, opcodes[i]);
                    for (int j = 0; j < opcodes.length; j++) {
                        remove(opcodes[j], c);
                    }
                }
            }
        }

        lineCount = lineCount + 2;

        int[] r = new int[4];

        for (; lineCount < lines.length; lineCount++) {
            String[] split = lines[lineCount].split("[^0-9]+");
            byte[] instruction = {(byte) Integer.parseInt(split[0]), (byte) Integer.parseInt(split[1]),
                    (byte) Integer.parseInt(split[2]), (byte) Integer.parseInt(split[3])};

            r = runOp(orderedOpCodes.get(instruction[0]), r, instruction);
        }

        System.out.println(r[0]);
    }

    static void remove(OP op, byte c) {
        int i = -1;
        for (int j = 0; j < op.matchCount.length; j++) {
            if (c == op.matchCount[j]) {
                i = j;
            }
        }
        if (i != -1) {
            byte[] temp = new byte[op.matchCount.length - 1];
            int k = 0;
            for (int j = 0; j < op.matchCount.length; j++) {
                if (j != i) {
                    temp[k++] = op.matchCount[j];
                }
            }
            op.matchCount = temp;
        }
    }

    static void add(OP op, byte c) {
        for (byte v : op.matchCount) {
            if (v == c) {
                return;
            }
        }
        byte[] temp = new byte[op.matchCount.length + 1];
        System.arraycopy(op.matchCount, 0, temp, 0, op.matchCount.length);
        temp[op.matchCount.length] = c;
        op.matchCount = temp;
    }

    static int testCode(int[] registers, int[] result, byte[] instruction, OP[] opcodes) {
        int sum = 0;
        for (OP opcode : opcodes) {
            if (match(result, runOp(opcode, registers, instruction))) {
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
        int[] registerCP = registers.clone();
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

    static class OP {
        char a;
        char b;
        char action;
        String name;
        byte[] matchCount;

        public OP(String name, char action, char a, char b) {
            this.name = name;
            this.action = action;
            this.a = a;
            this.b = b;
            this.matchCount = new byte[0];
        }
    }

    static int strToInt(String s) {
        return Integer.parseInt(s);
    }
}
