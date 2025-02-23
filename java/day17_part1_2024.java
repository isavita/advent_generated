
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Solution {
  public static void main(String[] args) throws IOException {
    BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
    int A = 0, B = 0, C = 0;
    List<Integer> program = new ArrayList<>();
    String line;
    while ((line = reader.readLine()) != null) {
      line = line.trim();
      if (line.startsWith("Register A:")) {
        A = Integer.parseInt(line.split(":")[1].trim());
      } else if (line.startsWith("Register B:")) {
        B = Integer.parseInt(line.split(":")[1].trim());
      } else if (line.startsWith("Register C:")) {
        C = Integer.parseInt(line.split(":")[1].trim());
      } else if (line.startsWith("Program:")) {
        String[] parts = line.split(":")[1].trim().split(",");
        for (String part : parts) {
          program.add(Integer.parseInt(part.trim()));
        }
      }
    }
    reader.close();
    List<Integer> outputVals = new ArrayList<>();
    int ip = 0;
    while (ip < program.size() && ip + 1 < program.size()) {
      int opcode = program.get(ip);
      int operand = program.get(ip + 1);
      switch (opcode) {
        case 0:
          int den = getComboVal(operand, A, B, C);
          A = den != 0 ? A >> den : 0;
          ip += 2;
          break;
        case 1:
          B ^= operand;
          ip += 2;
          break;
        case 2:
          B = getComboVal(operand, A, B, C) % 8;
          ip += 2;
          break;
        case 3:
          ip = A != 0 ? operand : ip + 2;
          break;
        case 4:
          B ^= C;
          ip += 2;
          break;
        case 5:
          outputVals.add(getComboVal(operand, A, B, C) % 8);
          ip += 2;
          break;
        case 6:
          B = A >> getComboVal(operand, A, B, C);
          ip += 2;
          break;
        case 7:
          C = A >> getComboVal(operand, A, B, C);
          ip += 2;
          break;
        default:
          ip = program.size();
          break;
      }
    }
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < outputVals.size(); i++) {
      sb.append(outputVals.get(i));
      if (i < outputVals.size() - 1) {
        sb.append(",");
      }
    }
    System.out.println(sb.toString());
  }

  private static int getComboVal(int op, int A, int B, int C) {
    if (op <= 3) {
      return op;
    }
    if (op == 4) {
      return A;
    }
    if (op == 5) {
      return B;
    }
    if (op == 6) {
      return C;
    }
    throw new IllegalArgumentException("invalid combo operand");
  }
}
