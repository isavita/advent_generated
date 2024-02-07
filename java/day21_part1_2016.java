
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            StringBuilder input = new StringBuilder();
            while ((line = reader.readLine()) != null) {
                input.append(line).append("\n");
            }
            reader.close();

            String[] operations = input.toString().trim().split("\n");
            String password = "abcdefgh";

            for (String op : operations) {
                password = applyOperation(op, password);
            }

            System.out.println(password);

        } catch (IOException e) {
            System.out.println("Error reading input file: " + e.getMessage());
        }
    }

    public static String applyOperation(String op, String password) {
        String[] fields = op.split(" ");
        switch (fields[0]) {
            case "swap":
                switch (fields[1]) {
                    case "position":
                        int x = fields[2].charAt(0) - '0';
                        int y = fields[5].charAt(0) - '0';
                        password = swapPosition(password, x, y);
                        break;
                    case "letter":
                        char xChar = fields[2].charAt(0);
                        char yChar = fields[5].charAt(0);
                        password = swapLetter(password, xChar, yChar);
                        break;
                }
                break;
            case "rotate":
                switch (fields[1]) {
                    case "left":
                        int leftSteps = fields[2].charAt(0) - '0';
                        password = rotateLeft(password, leftSteps);
                        break;
                    case "right":
                        int rightSteps = fields[2].charAt(0) - '0';
                        password = rotateRight(password, rightSteps);
                        break;
                    case "based":
                        char xChar = fields[6].charAt(0);
                        password = rotateBasedOnPosition(password, xChar);
                        break;
                }
                break;
            case "reverse":
                int xRev = fields[2].charAt(0) - '0';
                int yRev = fields[4].charAt(0) - '0';
                password = reversePositions(password, xRev, yRev);
                break;
            case "move":
                int xMove = fields[2].charAt(0) - '0';
                int yMove = fields[5].charAt(0) - '0';
                password = movePosition(password, xMove, yMove);
                break;
        }
        return password;
    }

    public static String swapPosition(String password, int x, int y) {
        char[] chars = password.toCharArray();
        char temp = chars[x];
        chars[x] = chars[y];
        chars[y] = temp;
        return new String(chars);
    }

    public static String swapLetter(String password, char x, char y) {
        char[] chars = password.toCharArray();
        for (int i = 0; i < chars.length; i++) {
            if (chars[i] == x) {
                chars[i] = y;
            } else if (chars[i] == y) {
                chars[i] = x;
            }
        }
        return new String(chars);
    }

    public static String rotateLeft(String password, int steps) {
        steps = steps % password.length();
        return password.substring(steps) + password.substring(0, steps);
    }

    public static String rotateRight(String password, int steps) {
        steps = steps % password.length();
        return password.substring(password.length() - steps) + password.substring(0, password.length() - steps);
    }

    public static String rotateBasedOnPosition(String password, char x) {
        int index = password.indexOf(x);
        int steps = 1 + index;
        if (index >= 4) {
            steps++;
        }
        return rotateRight(password, steps);
    }

    public static String reversePositions(String password, int x, int y) {
        char[] chars = password.toCharArray();
        for (int i = x, j = y; i < j; i++, j--) {
            char temp = chars[i];
            chars[i] = chars[j];
            chars[j] = temp;
        }
        return new String(chars);
    }

    public static String movePosition(String password, int x, int y) {
        char[] chars = password.toCharArray();
        char temp = chars[x];
        StringBuilder builder = new StringBuilder(password);
        builder.deleteCharAt(x);
        builder.insert(y, temp);
        return builder.toString();
    }
}
