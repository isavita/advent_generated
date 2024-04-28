import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String movesStr = br.readLine();
        String[] moves = movesStr.split(",");

        char[] programs = "abcdefghijklmnop".toCharArray();

        for (String move : moves) {
            switch (move.charAt(0)) {
                case 's':
                    int x = Integer.parseInt(move.substring(1));
                    spin(programs, x);
                    break;
                case 'x':
                    String[] positions = move.substring(1).split("/");
                    int posA = Integer.parseInt(positions[0]);
                    int posB = Integer.parseInt(positions[1]);
                    exchange(programs, posA, posB);
                    break;
                case 'p':
                    char progA = move.charAt(1);
                    char progB = move.charAt(3);
                    partner(programs, progA, progB);
                    break;
            }
        }

        System.out.println(new String(programs));
    }

    static void spin(char[] programs, int x) {
        int n = programs.length;
        char[] temp = new char[n];
        System.arraycopy(programs, 0, temp, 0, n);
        for (int i = 0; i < n; i++) {
            programs[(i + x) % n] = temp[i];
        }
    }

    static void exchange(char[] programs, int A, int B) {
        char temp = programs[A];
        programs[A] = programs[B];
        programs[B] = temp;
    }

    static void partner(char[] programs, char A, char B) {
        int indexA = -1, indexB = -1;
        for (int i = 0; i < programs.length; i++) {
            if (programs[i] == A) {
                indexA = i;
            }
            if (programs[i] == B) {
                indexB = i;
            }
        }
        exchange(programs, indexA, indexB);
    }
}