import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Day1 {
    public static void main(String[] args) throws Exception {
        List<String> lines = Files.readAllLines(Paths.get("input.txt"));
        List<Integer> leftList = new ArrayList<>();
        Map<Integer, Integer> rightCounts = new HashMap<>();

        for (String line : lines) {
            if (line.trim().isEmpty()) continue;
            String[] parts = line.trim().split("\\s+");
            leftList.add(Integer.parseInt(parts[0]));
            
            int rightNum = Integer.parseInt(parts[parts.length - 1]);
            rightCounts.merge(rightNum, 1, Integer::sum);
        }

        long similarityScore = 0;
        for (int num : leftList) {
            similarityScore += (long) num * rightCounts.getOrDefault(num, 0);
        }

        System.out.println("Similarity score: " + similarityScore);
    }
}
