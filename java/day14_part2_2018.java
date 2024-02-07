
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String input = reader.readLine();
            reader.close();

            List<Integer> scoreboard = new ArrayList<>();
            scoreboard.add(3);
            scoreboard.add(7);
            int elf1 = 0;
            int elf2 = 1;
            int inputLen = input.length();
            int[] inputSequence = new int[inputLen];

            for (int i = 0; i < inputLen; i++) {
                inputSequence[i] = Character.getNumericValue(input.charAt(i));
            }

            while (true) {
                int newScore = scoreboard.get(elf1) + scoreboard.get(elf2);
                if (newScore >= 10) {
                    scoreboard.add(newScore / 10);
                    if (checkSequence(scoreboard, inputSequence)) {
                        break;
                    }
                }
                scoreboard.add(newScore % 10);
                if (checkSequence(scoreboard, inputSequence)) {
                    break;
                }

                elf1 = (elf1 + scoreboard.get(elf1) + 1) % scoreboard.size();
                elf2 = (elf2 + scoreboard.get(elf2) + 1) % scoreboard.size();
            }

            System.out.println(scoreboard.size() - inputLen);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static boolean checkSequence(List<Integer> scoreboard, int[] sequence) {
        if (scoreboard.size() < sequence.length) {
            return false;
        }
        int start = scoreboard.size() - sequence.length;
        for (int i = 0; i < sequence.length; i++) {
            if (scoreboard.get(start + i) != sequence[i]) {
                return false;
            }
        }
        return true;
    }
}
