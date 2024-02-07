
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            StringBuilder input = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                input.append(line).append("\n");
            }
            reader.close();
            System.out.println(solve(input.toString().trim()));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static Map<String, List<String>> parseInput(String input) {
        String[] blocks = input.split("\n\n");
        List<String> startingMaterial = splitMolecules(blocks[1]);

        Map<String, List<String>> graph = new HashMap<>();

        for (String l : blocks[0].split("\n")) {
            String[] parts = l.split(" => ");
            if (!graph.containsKey(parts[0])) {
                graph.put(parts[0], new ArrayList<>());
            }
            graph.get(parts[0]).add(parts[1]);
        }

        return graph;
    }

    private static List<String> splitMolecules(String input) {
        List<String> molecules = new ArrayList<>();
        StringBuilder molecule = new StringBuilder();
        for (char c : input.toCharArray()) {
            int code = (int) c;
            if (code >= 65 && code <= 90) {
                if (molecule.length() > 0) {
                    molecules.add(molecule.toString());
                    molecule = new StringBuilder();
                }
            }
            molecule.append(c);
        }
        molecules.add(molecule.toString());

        return molecules;
    }

    private static int solve(String input) {
        Map<String, List<String>> reverseGraph = parseInput(input);

        Map<String, String> productToReactant = new HashMap<>();
        for (Map.Entry<String, List<String>> entry : reverseGraph.entrySet()) {
            String reactant = entry.getKey();
            for (String product : entry.getValue()) {
                if (productToReactant.containsKey(product)) {
                    throw new RuntimeException("dup found");
                }
                productToReactant.put(product, reactant);
            }
        }

        List<String> allProducts = new ArrayList<>(productToReactant.keySet());

        List<String> startingMols = splitMolecules(input.split("\n\n")[1]);
        String start = String.join("", startingMols);
        String mol = start;

        int steps = 0;
        Random rn = new Random();
        while (!mol.equals("e")) {
            boolean changeMade = false;
            for (String prod : allProducts) {
                int count = countSubstring(mol, prod);
                if (count <= 0) {
                    continue;
                }
                changeMade = true;
                steps += count;
                mol = mol.replace(prod, productToReactant.get(prod));
                break;
            }

            if (!changeMade) {
                allProducts = shuffleList(allProducts, rn);
                mol = start;
                steps = 0;
            }
        }

        return steps;
    }

    private static int countSubstring(String str, String substr) {
        return str.split(substr, -1).length - 1;
    }

    private static List<String> shuffleList(List<String> list, Random rn) {
        for (int i = 0; i < list.size(); i++) {
            int index = rn.nextInt(list.size());
            String temp = list.get(i);
            list.set(i, list.get(index));
            list.set(index, temp);
        }
        return list;
    }
}
