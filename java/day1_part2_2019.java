
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static int processLine(String line) {
        return Integer.parseInt(line.trim());
    }

    public static int getTotal(List<Integer> masses) {
        int total = 0;
        for (int mass : masses) {
            total += calcFuelMass(mass);
        }
        return total;
    }

    public static int calcFuelMass(int mass) {
        int fuel = (int) (Math.floor((double) mass / 3) - 2);
        if (fuel <= 0) {
            return 0;
        }
        return fuel + calcFuelMass(fuel);
    }

    public static void main(String[] args) {
        List<Integer> masses = new ArrayList<>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            while ((line = reader.readLine()) != null) {
                masses.add(processLine(line));
            }
            reader.close();
        } catch (IOException e) {
            System.out.println("Error reading file");
            return;
        }

        int total = getTotal(masses);
        System.out.println(total);
    }
}
