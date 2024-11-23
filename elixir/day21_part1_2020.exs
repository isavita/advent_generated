
defmodule AllergenAnalysis do
  def solve do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.reduce({%{}, %{}, %{}}, fn line, {allergen_map, ingredient_count, safe_ingredients} ->
      [ingredients_part, allergens_part] = String.split(line, " (contains ", parts: 2)
      
      ingredients = String.split(ingredients_part)
      allergens = if allergens_part, do: String.trim_trailing(allergens_part, ")") |> String.split(", "), else: []
      
      new_ingredient_count = Enum.reduce(ingredients, ingredient_count, fn ing, acc ->
        Map.update(acc, ing, 1, &(&1 + 1))
      end)
      
      new_safe_ingredients = Enum.reduce(ingredients, safe_ingredients, fn ing, acc ->
        Map.put(acc, ing, true)
      end)
      
      new_allergen_map = Enum.reduce(allergens, allergen_map, fn allergen, acc ->
        Map.update(acc, allergen, MapSet.new(ingredients), fn existing ->
          MapSet.intersection(existing, MapSet.new(ingredients))
        end)
      end)
      
      {new_allergen_map, new_ingredient_count, new_safe_ingredients}
    end)
    |> then(fn {allergen_map, ingredient_count, safe_ingredients} ->
      dangerous_ingredients = allergen_map 
      |> Map.values() 
      |> Enum.reduce(MapSet.new(), &MapSet.union/2)
      
      safe_ingredients = Map.keys(safe_ingredients) 
      |> MapSet.new() 
      |> MapSet.difference(dangerous_ingredients)
      
      Enum.reduce(safe_ingredients, 0, fn ing, acc -> 
        acc + Map.get(ingredient_count, ing, 0) 
      end)
    end)
    |> IO.puts()
  end
end

AllergenAnalysis.solve()
