
import java.io.*;
import java.util.*;

public class Garden {

  static class Complex {
    int real;
    int imag;

    public Complex(int real, int imag) {
      this.real = real;
      this.imag = imag;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;
      Complex complex = (Complex) o;
      return real == complex.real && imag == complex.imag;
    }

    @Override
    public int hashCode() {
      return Objects.hash(real, imag);
    }

    public Complex add(Complex other) {
      return new Complex(this.real + other.real, this.imag + other.imag);
    }
  }

  static Map<Complex, Boolean> garden = new HashMap<>();
  static Complex start = null;
  static int maxSize;

  static Complex complexMod(Complex num, int mod) {
    return new Complex((num.real % mod + mod) % mod, (num.imag % mod + mod) % mod);
  }

  static long calculateNumEnds(int numIterations) {
    Set<Complex> queue = new HashSet<>();
    queue.add(start);
    List<Integer> done = new ArrayList<>();

    for (int i = 0; i < 3 * maxSize; i++) {
      if (i % maxSize == (maxSize - 1) / 2) {
        done.add(queue.size());
      }
      if (done.size() == 3) {
        break;
      }

      Set<Complex> newQueue = new HashSet<>();
      for (Complex point : queue) {
        int[][] dirs = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};
        for (int[] dir : dirs) {
          Complex next = point.add(new Complex(dir[0], dir[1]));
          if (garden.containsKey(complexMod(next, maxSize))) {
            newQueue.add(next);
          }
        }
      }
      queue = newQueue;
    }

    long a = done.get(0);
    long b = done.get(1);
    long c = done.get(2);
    long n = numIterations / maxSize;

    return a + n * (b - a + ((n - 1) * (c - 2 * b + a) / 2));
  }

  public static void main(String[] args) throws IOException {
    BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
    List<String> lines = new ArrayList<>();
    String line;
    while ((line = reader.readLine()) != null) {
      lines.add(line);
    }
    reader.close();

    int y = 0;
    for (String row : lines) {
      for (int x = 0; x < row.length(); x++) {
        char c = row.charAt(x);
        if (c != '#') {
          garden.put(new Complex(x, y), true);
          if (c == 'S') {
            start = new Complex(x, y);
          }
        }
      }
      y++;
    }
    maxSize = lines.size();

    System.out.println(calculateNumEnds(26501365));
  }
}
