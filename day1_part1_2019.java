
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    static List<Integer> masses = new ArrayList<Integer>();
    static double total = 0;

    public static void processLine(String line) {
        int m = Integer.parseInt(line.trim());
        masses.add(m);
    }

    public static void getTotal() {
        double tempTotal = 0;

        for (int i = 0; i < masses.size(); i++) {
            tempTotal += (Math.floor((double) masses.get(i) / 3) - 2);
        }

        total = tempTotal;
        return;
    }

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            while ((line = reader.readLine()) != null) {
                processLine(line);
            }
            reader.close();
        } catch (IOException e) {
            System.out.println("Error reading file");
        }

        getTotal();

        System.out.println(total);
    }
}
