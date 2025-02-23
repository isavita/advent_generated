
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.IntSupplier;

public class ArcadeGame {

  static class IntcodeComputer {

    private final Map<Long, Long> memory = new HashMap<>();
    private long ip = 0;
    private long relativeBase = 0;

    public IntcodeComputer(long[] program) {
      for (long i = 0; i < program.length; i++) {
        memory.put(i, program[(int) i]);
      }
    }

    private long getParameter(long offset, long mode) {
      long address =
          switch ((int) mode) {
            case 0 -> memory.getOrDefault(ip + offset, 0L);
            case 1 -> ip + offset;
            case 2 -> relativeBase + memory.getOrDefault(ip + offset, 0L);
            default -> throw new IllegalArgumentException("Invalid parameter mode");
          };
      return memory.getOrDefault(address, 0L);
    }

    private long getWriteAddress(long offset, long mode) {
      return switch ((int) mode) {
        case 0 -> memory.getOrDefault(ip + offset, 0L);
        case 2 -> relativeBase + memory.getOrDefault(ip + offset, 0L);
        default -> throw new IllegalArgumentException("Invalid write address mode");
      };
    }

    public Iterator<Long> execute(IntSupplier inputProvider) {
      return new Iterator<>() {
        @Override
        public boolean hasNext() {
          return memory.getOrDefault(ip, 0L) % 100 != 99;
        }

        @Override
        public Long next() {
          while (true) {
            long instruction = memory.getOrDefault(ip, 0L);
            long opcode = instruction % 100;
            long[] modes = {
              (instruction / 100) % 10, (instruction / 1000) % 10, (instruction / 10000) % 10
            };

            if (opcode == 99) {
              return null;
            }

            if (opcode == 3) {
              long address = getWriteAddress(1, modes[0]);
              memory.put(address, (long) inputProvider.getAsInt());
              ip += 2;
            } else if (opcode == 4) {
              long outputValue = getParameter(1, modes[0]);
              ip += 2;
              return outputValue;
            } else {
              long param1 = getParameter(1, modes[0]);
              Long param2 = (opcode != 9) ? getParameter(2, modes[1]) : null;

              if (opcode == 1 || opcode == 2) {
                long result = (opcode == 1) ? param1 + param2 : param1 * param2;
                memory.put(getWriteAddress(3, modes[2]), result);
                ip += 4;
              } else if (opcode == 5 || opcode == 6) {
                if ((opcode == 5 && param1 != 0) || (opcode == 6 && param1 == 0)) {
                  ip = param2;
                } else {
                  ip += 3;
                }
              } else if (opcode == 7 || opcode == 8) {
                long result = (opcode == 7 && param1 < param2) || (opcode == 8 && param1 == param2)
                    ? 1
                    : 0;
                memory.put(getWriteAddress(3, modes[2]), result);
                ip += 4;
              } else if (opcode == 9) {
                relativeBase += param1;
                ip += 2;
              }
            }
          }
        }
      };
    }
  }

  public static long playGame(long[] program) {
    IntcodeComputer computer = new IntcodeComputer(program);
    computer.memory.put(0L, 2L);
    Iterator<Long> outputs = computer.execute(() -> (ballX > paddleX) ? 1 : ((ballX < paddleX) ? -1 : 0));

    long score = 0;

    while (outputs.hasNext()) {
      Long x = outputs.next();
      if (!outputs.hasNext()) break;
      Long y = outputs.next();
      if (!outputs.hasNext()) break;
      Long tileId = outputs.next();
      if (x == -1 && y == 0) {
        score = tileId;
      } else {
        if (tileId == 3) {
          paddleX = x;
        } else if (tileId == 4) {
          ballX = x;
        }
      }
    }
    return score;
  }

  static long paddleX = 0;
  static long ballX = 0;

  public static void main(String[] args) throws IOException {
    String input = new String(Files.readAllBytes(Paths.get("input.txt"))).trim();
    long[] program =
        java.util.Arrays.stream(input.split(",")).mapToLong(Long::parseLong).toArray();
    System.out.println("Final Score: " + playGame(program));
  }
}
