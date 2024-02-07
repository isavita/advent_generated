
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Solution {
    public static void main(String[] args) {
        List<Integer> positions = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] numbers = line.split(",");
                for (String num_str : numbers) {
                    int num = Integer.parseInt(num_str);
                    positions.add(num);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
            return;
        }

        Collections.sort(positions);

        int min_fuel = Integer.MAX_VALUE;
        for (int i = positions.get(0); i <= positions.get(positions.size() - 1); i++) {
            int fuel = 0;
            for (int pos : positions) {
                fuel += calculateFuel(pos, i);
            }
            if (fuel < min_fuel) {
                min_fuel = fuel;
            }
        }
        System.out.println(min_fuel);
    }

    public static int calculateFuel(int currentPosition, int newPosition) {
        return Math.abs(currentPosition - newPosition);
    }
}
