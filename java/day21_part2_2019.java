
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class VM {
  private Map<Long, Long> code = new HashMap<>();
  private long ip = 0;
  private List<Long> input = new ArrayList<>();
  private List<Long> output = new ArrayList<>();
  private long relativeBase = 0;

  public VM(String filename) throws IOException {
    load(filename);
  }

  private void load(String filename) throws IOException {
    String[] listStr = Files.readString(Paths.get(filename)).trim().split(",");
    for (int i = 0; i < listStr.length; i++) {
      code.put((long) i, Long.parseLong(listStr[i]));
    }
  }

  public void run() {
    while (true) {
      long cmd = code.getOrDefault(ip, 0L);
      int opcode = (int) (cmd % 100);
      int[] modes = {(int) ((cmd / 100) % 10), (int) ((cmd / 1000) % 10), (int) ((cmd / 10000) % 10)};

      java.util.function.Function<Integer, Long> getParam =
          (index) -> {
            int mode = modes[index - 1];
            long value = code.getOrDefault(ip + index, 0L);
            if (mode == 0) {
              return code.getOrDefault(value, 0L);
            } else if (mode == 1) {
              return value;
            } else {
              return code.getOrDefault(relativeBase + value, 0L);
            }
          };

      java.util.function.Function<Integer, Long> getAddress =
          (index) -> {
            int mode = modes[index - 1];
            long value = code.getOrDefault(ip + index, 0L);
            if (mode == 2) {
              return relativeBase + value;
            }
            return value;
          };

      if (opcode == 1) {
        code.put(getAddress.apply(3), getParam.apply(1) + getParam.apply(2));
        ip += 4;
      } else if (opcode == 2) {
        code.put(getAddress.apply(3), getParam.apply(1) * getParam.apply(2));
        ip += 4;
      } else if (opcode == 3) {
        code.put(getAddress.apply(1), input.remove(0));
        ip += 2;
      } else if (opcode == 4) {
        output.add(getParam.apply(1));
        ip += 2;
      } else if (opcode == 5) {
        if (getParam.apply(1) != 0) {
          ip = getParam.apply(2);
        } else {
          ip += 3;
        }
      } else if (opcode == 6) {
        if (getParam.apply(1) == 0) {
          ip = getParam.apply(2);
        } else {
          ip += 3;
        }
      } else if (opcode == 7) {
        code.put(getAddress.apply(3), (getParam.apply(1) < getParam.apply(2)) ? 1L : 0L);
        ip += 4;
      } else if (opcode == 8) {
        code.put(getAddress.apply(3), (getParam.apply(1).equals(getParam.apply(2))) ? 1L : 0L);
        ip += 4;
      } else if (opcode == 9) {
        relativeBase += getParam.apply(1);
        ip += 2;
      } else if (opcode == 99) {
        break;
      } else {
        throw new RuntimeException("Unknown opcode " + opcode);
      }
    }
  }

  public void sendString(String s) {
    for (char c : s.toCharArray()) {
      input.add((long) c);
    }
    input.add(10L);
  }

  public static void main(String[] args) throws IOException {
    VM vm = new VM("input.txt");
    String[] instructions = {
      "NOT A J", "NOT B T", "OR T J", "NOT C T", "OR T J", "AND D J", "NOT A T", "AND A T",
      "OR E T", "OR H T", "AND T J", "RUN"
    };
    for (String i : instructions) {
      vm.sendString(i);
    }
    vm.run();
    for (long output : vm.output) {
      System.out.println(output);
    }
  }
}
