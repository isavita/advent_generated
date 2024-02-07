
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {

    static class Scrambler {
        private char[] pw;

        public Scrambler(String pw) {
            this.pw = pw.toCharArray();
        }

        public String toString() {
            return new String(pw);
        }

        public void swapPositions(int x, int y) {
            char temp = pw[x];
            pw[x] = pw[y];
            pw[y] = temp;
        }

        public void swapLetters(char x, char y) {
            swapPositions(new String(pw).indexOf(x), new String(pw).indexOf(y));
        }

        public void rotate(int steps) {
            int length = pw.length;
            steps = steps % length;
            if (steps < 0) {
                steps += length;
            }
            char[] newPw = new char[length];
            System.arraycopy(pw, length - steps, newPw, 0, steps);
            System.arraycopy(pw, 0, newPw, steps, length - steps);
            pw = newPw;
        }

        public void rotateLetter(char x) {
            int index = new String(pw).indexOf(x);
            if (index >= 4) {
                index++;
            }
            rotate(index + 1);
        }

        public void derotateLetter(char x) {
            int index = new String(pw).indexOf(x);
            int rot;
            if (index % 2 == 1) {
                rot = -(index + 1) / 2;
            } else if (index != 0) {
                rot = (6 - index) / 2;
            } else {
                rot = -1;
            }
            rotate(rot);
        }

        public void reverse(int x, int y) {
            while (x < y) {
                char temp = pw[x];
                pw[x] = pw[y];
                pw[y] = temp;
                x++;
                y--;
            }
        }

        public void move(int x, int y) {
            char ch = pw[x];
            if (x < y) {
                System.arraycopy(pw, x + 1, pw, x, y - x);
            } else {
                System.arraycopy(pw, y, pw, y + 1, x - y);
            }
            pw[y] = ch;
        }

        public Scrambler scramble(List<String> instructions, int direction) {
            if (direction < 0) {
                reverseStrings(instructions);
            }
            for (String instruction : instructions) {
                String[] line = instruction.split(" ");
                if (instruction.startsWith("swap")) {
                    String x = line[2];
                    String y = line[line.length - 1];
                    if (line[1].equals("position")) {
                        int xi = Integer.parseInt(x);
                        int yi = Integer.parseInt(y);
                        swapPositions(xi, yi);
                    } else {
                        swapLetters(x.charAt(0), y.charAt(0));
                    }
                } else if (instruction.startsWith("rotate")) {
                    if (line[1].equals("based")) {
                        if (direction > 0) {
                            rotateLetter(line[line.length - 1].charAt(0));
                        } else {
                            derotateLetter(line[line.length - 1].charAt(0));
                        }
                    } else {
                        int x = Integer.parseInt(line[2]);
                        if (line[1].equals("left")) {
                            x = -x;
                        }
                        if (direction < 0) {
                            x = -x;
                        }
                        rotate(x);
                    }
                } else if (instruction.startsWith("reverse")) {
                    String x = line[2];
                    String y = line[line.length - 1];
                    int xi = Integer.parseInt(x);
                    int yi = Integer.parseInt(y);
                    reverse(xi, yi);
                } else if (instruction.startsWith("move")) {
                    String x = line[2];
                    String y = line[line.length - 1];
                    int xi = Integer.parseInt(x);
                    int yi = Integer.parseInt(y);
                    if (direction < 0) {
                        int temp = xi;
                        xi = yi;
                        yi = temp;
                    }
                    move(xi, yi);
                }
            }
            return this;
        }

        public Scrambler unscramble(List<String> instructions) {
            return scramble(instructions, -1);
        }

        private void reverseStrings(List<String> s) {
            for (int i = 0, j = s.size() - 1; i < j; i++, j--) {
                String temp = s.get(i);
                s.set(i, s.get(j));
                s.set(j, temp);
            }
        }
    }

    public static void main(String[] args) {
        List<String> instructions = new ArrayList<>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            while ((line = reader.readLine()) != null) {
                instructions.add(line);
            }
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        String hashed = "fbgdceah";
        Scrambler scrambler = new Scrambler(hashed);
        Scrambler result = scrambler.unscramble(instructions);
        System.out.println(result);
    }
}
