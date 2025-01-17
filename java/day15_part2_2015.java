
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
        long maxScore = 0;
        long maxScore500Calories = 0;

        if (ingredients.size() == 2) {
            for (int i = 0; i <= 100; i++) {
                int j = 100 - i;
                long score = calculateScore(ingredients, new int[]{i, j});
                maxScore = Math.max(maxScore, score);
                if (calculateCalories(ingredients, new int[]{i, j}) == 500) {
                    maxScore500Calories = Math.max(maxScore500Calories, score);
                }
            }
        } else if (ingredients.size() == 4) {
            for (int i = 0; i <= 100; i++) {
                for (int j = 0; j <= 100 - i; j++) {
                    for (int k = 0; k <= 100 - i - j; k++) {
                        int l = 100 - i - j - k;
                        long score = calculateScore(ingredients, new int[]{i, j, k, l});
                        maxScore = Math.max(maxScore, score);
                        if (calculateCalories(ingredients, new int[]{i, j, k, l}) == 500) {
                            maxScore500Calories = Math.max(maxScore500Calories, score);
                        }
                    }
                }
            }
        }

        System.out.println(maxScore);
        System.out.println(maxScore500Calories);
    }

    static long calculateScore(List<Ingredient> ingredients, int[] amounts) {
        long capacity = 0, durability = 0, flavor = 0, texture = 0;
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

    static int calculateCalories(List<Ingredient> ingredients, int[] amounts) {
        int calories = 0;
        for (int i = 0; i < ingredients.size(); i++) {
            calories += ingredients.get(i).calories * amounts[i];
        }
        return calories;
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
}
