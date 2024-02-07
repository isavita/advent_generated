import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            int ipBind = 0;
            String line = reader.readLine();
            ipBind = Integer.parseInt(line.substring(4));

            String[][] instructions = new String[1000][4];
            int index = 0;
            while ((line = reader.readLine()) != null) {
                instructions[index] = line.split(" ");
                index++;
            }

            int[] registers = new int[6];
            for (int ip = 0; ip < index; ip++) {
                registers[ipBind] = ip;
                String[] inst = instructions[ip];
                String opcode = inst[0];
                int a = Integer.parseInt(inst[1]);
                int b = Integer.parseInt(inst[2]);
                int c = Integer.parseInt(inst[3]);

                switch (opcode) {
                    case "addr":
                        registers[c] = registers[a] + registers[b];
                        break;
                    case "addi":
                        registers[c] = registers[a] + b;
                        break;
                    case "mulr":
                        registers[c] = registers[a] * registers[b];
                        break;
                    case "muli":
                        registers[c] = registers[a] * b;
                        break;
                    case "banr":
                        registers[c] = registers[a] & registers[b];
                        break;
                    case "bani":
                        registers[c] = registers[a] & b;
                        break;
                    case "borr":
                        registers[c] = registers[a] | registers[b];
                        break;
                    case "bori":
                        registers[c] = registers[a] | b;
                        break;
                    case "setr":
                        registers[c] = registers[a];
                        break;
                    case "seti":
                        registers[c] = a;
                        break;
                    case "gtir":
                        registers[c] = boolToInt(a > registers[b]);
                        break;
                    case "gtri":
                        registers[c] = boolToInt(registers[a] > b);
                        break;
                    case "gtrr":
                        registers[c] = boolToInt(registers[a] > registers[b]);
                        break;
                    case "eqir":
                        registers[c] = boolToInt(a == registers[b]);
                        break;
                    case "eqri":
                        registers[c] = boolToInt(registers[a] == b);
                        break;
                    case "eqrr":
                        registers[c] = boolToInt(registers[a] == registers[b]);
                        break;
                }

                ip = registers[ipBind];
                if (ip < 0 || ip >= index) {
                    break;
                }
            }

            System.out.println(registers[0]);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static int boolToInt(boolean b) {
        if (b) {
            return 1;
        } else {
            return 0;
        }
    }
}