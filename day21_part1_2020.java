
import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            HashMap<String, HashMap<String, Boolean>> allergenMap = new HashMap<>();
            HashMap<String, Integer> ingredientCount = new HashMap<>();
            HashSet<String> safeIngredients = new HashSet<>();

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                String[] parts = line.split(" \\(contains ");
                String[] ingredients = parts[0].split(" ");
                String[] allergens = new String[0];
                if (parts.length > 1) {
                    allergens = parts[1].substring(0, parts[1].length() - 1).split(", ");
                }

                for (String ingredient : ingredients) {
                    ingredientCount.put(ingredient, ingredientCount.getOrDefault(ingredient, 0) + 1);
                    safeIngredients.add(ingredient);
                }

                for (String allergen : allergens) {
                    if (!allergenMap.containsKey(allergen)) {
                        allergenMap.put(allergen, new HashMap<>());
                        for (String ingredient : ingredients) {
                            allergenMap.get(allergen).put(ingredient, true);
                        }
                    } else {
                        for (String ingredient : new HashSet<>(allergenMap.get(allergen).keySet())) {
                            if (!contains(ingredients, ingredient)) {
                                allergenMap.get(allergen).remove(ingredient);
                            }
                        }
                    }
                }
            }

            for (HashMap<String, Boolean> ingredients : allergenMap.values()) {
                for (String ingredient : new HashSet<>(ingredients.keySet())) {
                    safeIngredients.remove(ingredient);
                }
            }

            int count = 0;
            for (String ingredient : safeIngredients) {
                count += ingredientCount.get(ingredient);
            }

            System.out.println(count);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static boolean contains(String[] arr, String str) {
        for (String s : arr) {
            if (s.equals(str)) {
                return true;
            }
        }
        return false;
    }
}
