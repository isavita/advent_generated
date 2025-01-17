
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class CookieRecipe {

    static class Ingredient {
        String name;
        int capacity;
        int durability;
        int flavor;
        int texture;
        int calories;

        public Ingredient(String name, int capacity, int durability, int flavor, int texture, int calories) {
            this.name = name;
            this.capacity = capacity;
            this.durability = durability;
            this.flavor = flavor;
            this.texture = texture;
            this.calories = calories;
        }
    }

    public static void main(String[] args) {
        List<Ingredient> ingredients = readIngredients("input.txt");
        long maxScore = findMaxScore(ingredients);
        System.out.println(maxScore);
    }

    static List<Ingredient> readIngredients(String filename) {
        List<Ingredient> ingredients = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split(" ");
                String name = parts[0].substring(0, parts[0].length() - 1);
                int capacity = Integer.parseInt(parts[2].substring(0, parts[2].length() - 1));
                int durability = Integer.parseInt(parts[4].substring(0, parts[4].length() - 1));
                int flavor = Integer.parseInt(parts[6].substring(0, parts[6].length() - 1));
                int texture = Integer.parseInt(parts[8].substring(0, parts[8].length() - 1));
                int calories = Integer.parseInt(parts[10]);
                ingredients.add(new Ingredient(name, capacity, durability, flavor, texture, calories));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return ingredients;
    }

    static long findMaxScore(List<Ingredient> ingredients) {
        long maxScore = 0;
        int n = ingredients.size();
        int[] amounts = new int[n];

        if (n == 2) {
            for (int i = 0; i <= 100; i++) {
                amounts[0] = i;
                amounts[1] = 100 - i;
                maxScore = Math.max(maxScore, calculateScore(ingredients, amounts));
            }
        } else if (n == 3) {
            for (int i = 0; i <= 100; i++) {
                for (int j = 0; j <= 100 - i; j++) {
                    amounts[0] = i;
                    amounts[1] = j;
                    amounts[2] = 100 - i - j;
                    maxScore = Math.max(maxScore, calculateScore(ingredients, amounts));
                }
            }
        } else if (n == 4) {
            for (int i = 0; i <= 100; i++) {
                for (int j = 0; j <= 100 - i; j++) {
                    for (int k = 0; k <= 100 - i - j; k++) {
                        amounts[0] = i;
                        amounts[1] = j;
                        amounts[2] = k;
                        amounts[3] = 100 - i - j - k;
                        maxScore = Math.max(maxScore, calculateScore(ingredients, amounts));
                    }
                }
            }
        }

        return maxScore;
    }

    static long calculateScore(List<Ingredient> ingredients, int[] amounts) {
        long capacity = 0;
        long durability = 0;
        long flavor = 0;
        long texture = 0;

        for (int i = 0; i < ingredients.size(); i++) {
            capacity += (long) ingredients.get(i).capacity * amounts[i];
            durability += (long) ingredients.get(i).durability * amounts[i];
            flavor += (long) ingredients.get(i).flavor * amounts[i];
            texture += (long) ingredients.get(i).texture * amounts[i];
        }

        capacity = Math.max(0, capacity);
        durability = Math.max(0, durability);
        flavor = Math.max(0, flavor);
        texture = Math.max(0, texture);

        return capacity * durability * flavor * texture;
    }
}
