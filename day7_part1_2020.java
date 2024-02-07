
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class solution {

    public static void main(String[] args) {
        Map<String, List<String>> contains = new HashMap<>();
        try {
            BufferedReader br = new BufferedReader(new FileReader("input.txt"));
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split(" bags contain ");
                String container = parts[0];
                if (parts[1].equals("no other bags.")) {
                    continue;
                }
                String[] containedBags = parts[1].split(", ");
                for (String bag : containedBags) {
                    String bagName = String.join(" ", Arrays.copyOfRange(bag.split(" "), 1, 3));
                    if (!contains.containsKey(bagName)) {
                        contains.put(bagName, new ArrayList<>());
                    }
                    contains.get(bagName).add(container);
                }
            }
            br.close();
        } catch (IOException e) {
            System.out.println("Error opening file: " + e);
            return;
        }

        int count = countCanContain("shiny gold", contains);
        System.out.println(count);
    }

    public static int countCanContain(String target, Map<String, List<String>> contains) {
        Set<String> seen = new HashSet<>();
        dfs(target, contains, seen);
        return seen.size();
    }

    public static void dfs(String bag, Map<String, List<String>> contains, Set<String> seen) {
        if (!contains.containsKey(bag)) {
            return;
        }
        for (String outer : contains.get(bag)) {
            if (!seen.contains(outer)) {
                seen.add(outer);
                dfs(outer, contains, seen);
            }
        }
    }
}
