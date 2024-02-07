
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {

    public static void findCombinations(List<Integer> containers, int target, int index, int count, int[] minCount, int[] ways) {
        if (target == 0) {
            if (minCount[0] == 0 || count < minCount[0]) {
                minCount[0] = count;
                ways[0] = 1;
            } else if (count == minCount[0]) {
                ways[0]++;
            }
            return;
        }
        if (target < 0 || index >= containers.size()) {
            return;
        }
        // Include current container
        findCombinations(containers, target - containers.get(index), index + 1, count + 1, minCount, ways);
        // Exclude current container
        findCombinations(containers, target, index + 1, count, minCount, ways);
    }

    public static void main(String[] args) {
        List<Integer> containers = new ArrayList<>();
        try {
            BufferedReader br = new BufferedReader(new FileReader("input.txt"));
            String line;
            while ((line = br.readLine()) != null) {
                containers.add(Integer.parseInt(line));
            }
            br.close();
        } catch (IOException e) {
            System.out.println("Error reading file: " + e);
            return;
        }

        int[] minCount = {0};
        int[] ways = {0};
        findCombinations(containers, 150, 0, 0, minCount, ways);
        System.out.println(ways[0]);
    }
}
