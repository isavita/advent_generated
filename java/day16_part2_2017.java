import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
        String line = reader.readLine();
        String[] moves = line.split(",");
        char[] programs = "abcdefghijklmnop".toCharArray();
        String initial = new String(programs);
        int cycleLen = 0;

        for (int i = 0; i < 1000000000; i++) {
            for (String move : moves) {
                char moveType = move.charAt(0);
                switch (moveType) {
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

            if (new String(programs).equals(initial)) {
                cycleLen = i + 1;
                break;
            }
        }

        // Reset to initial state
        programs = "abcdefghijklmnop".toCharArray();

        // Perform the dance (10^9 mod cycleLen) times
        for (int i = 0; i < 1000000000 % cycleLen; i++) {
            for (String move : moves) {
                char moveType = move.charAt(0);
                switch (moveType) {
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
        }

        System.out.println(new String(programs));
    }

    public static void spin(char[] programs, int x) {
        int n = programs.length;
        char[] temp = new char[n];
        System.arraycopy(programs, 0, temp, 0, n);
        for (int i = 0; i < n; i++) {
            programs[(i + x) % n] = temp[i];
        }
    }

    public static void exchange(char[] programs, int A, int B) {
        char temp = programs[A];
        programs[A] = programs[B];
        programs[B] = temp;
    }

    public static void partner(char[] programs, char A, char B) {
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