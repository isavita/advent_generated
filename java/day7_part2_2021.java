
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class solution {

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            List<Integer> positions = new ArrayList<>();
            String line;
            while ((line = reader.readLine()) != null) {
                String[] numbers = line.split(",");
                for (String num_str : numbers) {
                    int num = Integer.parseInt(num_str);
                    positions.add(num);
                }
            }
            reader.close();

            Collections.sort(positions);

            int min_fuel = Integer.MAX_VALUE;
            for (int i = positions.get(0); i <= positions.get(positions.size() - 1); i++) {
                int fuel = 0;
                for (int pos : positions) {
                    fuel += calculateNewFuel(pos, i);
                }
                if (fuel < min_fuel) {
                    min_fuel = fuel;
                }
            }
            System.out.println(min_fuel);

        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }

    public static int calculateNewFuel(int currentPosition, int newPosition) {
        int diff = abs(currentPosition - newPosition);
        return (diff * (diff + 1)) / 2;
    }

    public static int abs(int n) {
        if (n < 0) {
            return -n;
        }
        return n;
    }
}
