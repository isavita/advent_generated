
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            int input = Integer.parseInt(reader.readLine());
            reader.close();

            List<Integer> scoreboard = new ArrayList<>();
            scoreboard.add(3);
            scoreboard.add(7);
            int elf1 = 0;
            int elf2 = 1;

            while (scoreboard.size() < input + 10) {
                int newScore = scoreboard.get(elf1) + scoreboard.get(elf2);
                if (newScore >= 10) {
                    scoreboard.add(newScore / 10);
                }
                scoreboard.add(newScore % 10);

                elf1 = (elf1 + scoreboard.get(elf1) + 1) % scoreboard.size();
                elf2 = (elf2 + scoreboard.get(elf2) + 1) % scoreboard.size();
            }

            for (int i = input; i < input + 10; i++) {
                System.out.print(scoreboard.get(i));
            }
            System.out.println();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
