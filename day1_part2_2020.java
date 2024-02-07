
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static void main(String[] args) {
        List<Integer> expenses = new ArrayList<>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            while ((line = reader.readLine()) != null) {
                expenses.add(Integer.parseInt(line));
            }
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        for (int i = 0; i < expenses.size(); i++) {
            for (int j = i + 1; j < expenses.size(); j++) {
                for (int k = j + 1; k < expenses.size(); k++) {
                    if (expenses.get(i) + expenses.get(j) + expenses.get(k) == 2020) {
                        System.out.println(expenses.get(i) * expenses.get(j) * expenses.get(k));
                        return;
                    }
                }
            }
        }
    }
}
