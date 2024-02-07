
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

class Solution {
    private static class Chemical {
        String name;
        int amount;

        public Chemical(String name, int amount) {
            this.name = name;
            this.amount = amount;
        }
    }

    private static Chemical parseChemical(String s) {
        String[] parts = s.split(" ");
        int amount = Integer.parseInt(parts[0]);
        return new Chemical(parts[1], amount);
    }

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));

            Map<String, Chemical> reactions = new HashMap<>();
            Map<String, Chemical[]> ingredients = new HashMap<>();

            String line;
            while ((line = reader.readLine()) != null) {
                String[] parts = line.split(" => ");
                Chemical output = parseChemical(parts[1]);
                String[] inputs = parts[0].split(", ");
                Chemical[] inputsArr = new Chemical[inputs.length];
                for (int i = 0; i < inputs.length; i++) {
                    inputsArr[i] = parseChemical(inputs[i]);
                }
                reactions.put(output.name, output);
                ingredients.put(output.name, inputsArr);
            }

            System.out.println(calculateOre("FUEL", 1, reactions, ingredients, new HashMap<>()));
            
            reader.close();
        } catch (IOException e) {
            System.out.println("Error opening file: " + e.getMessage());
        }
    }

    private static int calculateOre(String chem, int amount, Map<String, Chemical> reactions, Map<String, Chemical[]> ingredients, Map<String, Integer> surplus) {
        if (chem.equals("ORE")) {
            return amount;
        }

        if (surplus.containsKey(chem) && surplus.get(chem) >= amount) {
            surplus.put(chem, surplus.get(chem) - amount);
            return 0;
        }

        amount -= surplus.getOrDefault(chem, 0);
        surplus.put(chem, 0);
        Chemical reaction = reactions.get(chem);
        int times = (amount + reaction.amount - 1) / reaction.amount;
        int ore = 0;

        for (Chemical ingredient : ingredients.get(chem)) {
            ore += calculateOre(ingredient.name, ingredient.amount * times, reactions, ingredients, surplus);
        }

        surplus.put(chem, surplus.getOrDefault(chem, 0) + times * reaction.amount - amount);
        return ore;
    }
}
