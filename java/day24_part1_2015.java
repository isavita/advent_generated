
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

public class Main {
    public static void main(String[] args) throws IOException {
        List<String> lines = Files.readAllLines(Paths.get("input.txt"));
        int[] packages = lines.stream().mapToInt(Integer::parseInt).toArray();
        int totalWeight = 0;

        for (int weight : packages) {
            totalWeight += weight;
        }

        int targetWeight = totalWeight / 3;
        long bestQE = Long.MAX_VALUE;
        int bestLength = Integer.MAX_VALUE;

        for (int comb = 1; comb < (1 << packages.length); comb++) {
            int groupWeight = 0, groupLength = 0;
            long qe = 1;

            for (int i = 0; i < packages.length; i++) {
                if ((comb & (1 << i)) != 0) {
                    groupWeight += packages[i];
                    qe *= packages[i];
                    groupLength++;
                }
            }

            if (groupWeight == targetWeight && groupLength <= bestLength) {
                if (groupLength < bestLength || qe < bestQE) {
                    bestLength = groupLength;
                    bestQE = qe;
                }
            }
        }

        System.out.println(bestQE);
    }
}
