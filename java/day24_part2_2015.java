
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class SleighBalancer {

    private static long minQE = Long.MAX_VALUE;
    private static int minSize = Integer.MAX_VALUE;

    public static void main(String[] args) {
        List<Integer> packages = readPackages("input.txt");
        long sum = packages.stream().mapToLong(Integer::longValue).sum();

        // Part 1
        findMinQE(packages, sum / 3, new ArrayList<>(), 0);
        System.out.println("Part 1 - Quantum Entanglement: " + minQE);

        // Part 2
        minQE = Long.MAX_VALUE;
        minSize = Integer.MAX_VALUE;
        findMinQE(packages, sum / 4, new ArrayList<>(), 0);
        System.out.println("Part 2 - Quantum Entanglement: " + minQE);
    }

    private static List<Integer> readPackages(String filename) {
        List<Integer> packages = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                packages.add(Integer.parseInt(line));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        Collections.sort(packages, Collections.reverseOrder());
        return packages;
    }

    private static void findMinQE(List<Integer> packages, long targetWeight, List<Integer> currentGroup, int startIndex) {
        long currentWeight = currentGroup.stream().mapToLong(Integer::longValue).sum();
        if (currentWeight > targetWeight) {
            return;
        }

        if (currentWeight == targetWeight) {
            if (currentGroup.size() < minSize) {
                minSize = currentGroup.size();
                minQE = calculateQE(currentGroup);
            } else if (currentGroup.size() == minSize) {
                long qe = calculateQE(currentGroup);
                minQE = Math.min(minQE, qe);
            }
            return;
        }

        if (currentGroup.size() >= minSize) {
            return;
        }

        for (int i = startIndex; i < packages.size(); i++) {
            currentGroup.add(packages.get(i));
            findMinQE(packages, targetWeight, currentGroup, i + 1);
            currentGroup.remove(currentGroup.size() - 1);
        }
    }

    private static long calculateQE(List<Integer> group) {
        long qe = 1;
        for (int weight : group) {
            qe *= weight;
        }
        return qe;
    }
}
