
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class RecursiveCircus {

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            Map<String, Integer> weights = new HashMap<>();
            Map<String, List<String>> children = new HashMap<>();
            Set<String> allPrograms = new HashSet<>();
            Set<String> childPrograms = new HashSet<>();

            while ((line = reader.readLine()) != null) {
                String[] parts = line.split(" ");
                String name = parts[0];
                int weight = Integer.parseInt(parts[1].substring(1, parts[1].length() - 1));
                weights.put(name, weight);
                allPrograms.add(name);

                if (parts.length > 3) {
                    List<String> childList = new ArrayList<>();
                    for (int i = 3; i < parts.length; i++) {
                        String child = parts[i].replace(",", "");
                        childList.add(child);
                        childPrograms.add(child);
                    }
                    children.put(name, childList);
                }
            }
            reader.close();

            // Part 1: Find the bottom program
            String bottomProgram = null;
            for (String program : allPrograms) {
                if (!childPrograms.contains(program)) {
                    bottomProgram = program;
                    break;
                }
            }
            System.out.println("Part 1: Bottom program is " + bottomProgram);

            // Part 2: Find the unbalanced program and its correct weight
            findUnbalancedWeight(bottomProgram, weights, children);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static int findUnbalancedWeight(String program, Map<String, Integer> weights, Map<String, List<String>> children) {
        if (!children.containsKey(program)) {
            return weights.get(program);
        }

        List<String> childList = children.get(program);
        Map<Integer, List<String>> weightMap = new HashMap<>();

        for (String child : childList) {
            int childWeight = findUnbalancedWeight(child, weights, children);
            weightMap.computeIfAbsent(childWeight, k -> new ArrayList<>()).add(child);
        }

        if (weightMap.size() > 1) {
            int correctWeight = 0;
            int wrongWeight = 0;
            String wrongProgram = null;
            for (Map.Entry<Integer, List<String>> entry : weightMap.entrySet()) {
                if (entry.getValue().size() == 1) {
                    wrongWeight = entry.getKey();
                    wrongProgram = entry.getValue().get(0);
                } else {
                    correctWeight = entry.getKey();
                }
            }
            int diff = correctWeight - wrongWeight;
            System.out.println("Part 2: Program " + wrongProgram + " should have weight " + (weights.get(wrongProgram) + diff));
        }

        int totalWeight = weights.get(program);
        for (String child : childList) {
            totalWeight += findUnbalancedWeight(child, weights, children);
        }
        return totalWeight;
    }
}
