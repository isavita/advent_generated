
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) throws IOException {
        long sum = 0;
        Scanner scanner = new Scanner(new File("input.txt"));
        while (scanner.hasNextLine()) {
            sum += fromSnafu(scanner.nextLine());
        }
        System.out.println(toSnafu(sum));
        scanner.close();
    }

    private static long fromSnafu(String s) {
        long n = 0;
        for (int i = 0; i < s.length(); i++) {
            n *= 5;
            switch (s.charAt(i)) {
                case '=':
                    n -= 2;
                    break;
                case '-':
                    n--;
                    break;
                default:
                    n += s.charAt(i) - '0';
            }
        }
        return n;
    }

    private static String toSnafu(long n) {
        if (n == 0) return "0";
        StringBuilder sb = new StringBuilder();
        while (n > 0) {
            switch ((int)(n % 5)) {
                case 3:
                    n += 5;
                    sb.append('=');
                    break;
                case 4:
                    n += 5;
                    sb.append('-');
                    break;
                default:
                    sb.append((char)('0' + n % 5));
            }
            n /= 5;
        }
        return sb.reverse().toString();
    }
}
