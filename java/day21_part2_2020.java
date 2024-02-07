
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            Map<String, Map<String, Boolean>> allergenMap = new HashMap<>();
            Map<String, String> ingredientAllergen = new HashMap<>();

            String line;
            while ((line = reader.readLine()) != null) {
                String[] parts = line.split(" \\(contains ");
                String[] ingredients = parts[0].split(" ");
                List<String> allergens = new ArrayList<>();
                if (parts.length > 1) {
                    allergens = Arrays.asList(parts[1].substring(0, parts[1].length() - 1).split(", "));
                }

                for (String allergen : allergens) {
                    if (!allergenMap.containsKey(allergen)) {
                        allergenMap.put(allergen, new HashMap<>());
                        for (String ingredient : ingredients) {
                            allergenMap.get(allergen).put(ingredient, true);
                        }
                    } else {
                        Iterator<String> iterator = allergenMap.get(allergen).keySet().iterator();
                        while (iterator.hasNext()) {
                            String ingredient = iterator.next();
                            if (!contains(ingredients, ingredient)) {
                                iterator.remove();
                            }
                        }
                    }
                }
            }

            while (!allergenMap.isEmpty()) {
                for (Iterator<Map.Entry<String, Map<String, Boolean>>> iterator = allergenMap.entrySet().iterator(); iterator.hasNext();) {
                    Map.Entry<String, Map<String, Boolean>> entry = iterator.next();
                    if (entry.getValue().size() == 1) {
                        String ingredient = entry.getValue().keySet().iterator().next();
                        ingredientAllergen.put(entry.getKey(), ingredient);
                        removeIngredientFromAll(allergenMap, ingredient);
                        iterator.remove();
                    }
                }
            }

            List<String> allergens = new ArrayList<>(ingredientAllergen.keySet());
            Collections.sort(allergens);

            List<String> result = new ArrayList<>();
            for (String allergen : allergens) {
                result.add(ingredientAllergen.get(allergen));
            }

            System.out.println(String.join(",", result));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static boolean contains(String[] array, String str) {
        for (String s : array) {
            if (s.equals(str)) {
                return true;
            }
        }
        return false;
    }

    public static void removeIngredientFromAll(Map<String, Map<String, Boolean>> allergenMap, String ingredient) {
        for (Map<String, Boolean> ingredients : allergenMap.values()) {
            ingredients.remove(ingredient);
        }
    }
}
