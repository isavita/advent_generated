
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Solution {
    public static void main(String[] args) {
        List<String> lines = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        for (int i = 0; i < lines.size() - 1; i++) {
            for (int j = i + 1; j < lines.size(); j++) {
                int diff = 0;
                for (int k = 0; k < lines.get(i).length(); k++) {
                    if (lines.get(i).charAt(k) != lines.get(j).charAt(k)) {
                        diff++;
                        if (diff > 1) {
                            break;
                        }
                    }
                }
                if (diff == 1) {
                    StringBuilder common = new StringBuilder();
                    for (int k = 0; k < lines.get(i).length(); k++) {
                        if (lines.get(i).charAt(k) == lines.get(j).charAt(k)) {
                            common.append(lines.get(i).charAt(k));
                        }
                    }
                    System.out.println(common.toString());
                    return;
                }
            }
        }
    }
}
