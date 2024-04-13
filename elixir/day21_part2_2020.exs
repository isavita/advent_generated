defmodule AllergenAssessment do
  def run do
    "input.txt"
    |> File.stream!()
    |> Enum.reduce(%{}, fn line, acc ->
      [ingredients_part, allergens_part] = String.split(line, " (contains ", parts: 2)
      ingredients = String.split(ingredients_part)
      allergens = String.trim_trailing(allergens_part, ")\n") |> String.split(", ")

      Enum.reduce(allergens, acc, fn allergen, acc_inner ->
        Map.update(acc_inner, allergen, MapSet.new(ingredients), fn ingr_set ->
          if MapSet.size(ingr_set) == 0, do: MapSet.new(ingredients), else: MapSet.intersection(ingr_set, MapSet.new(ingredients))
        end)
      end)
    end)
    |> resolve_allergens(%{})
    |> Enum.sort()
    |> Enum.map(&elem(&1, 1))
    |> Enum.join(",")
    |> IO.puts()
  end

  defp resolve_allergens(allergen_map, resolved) do
    found = Enum.find(allergen_map, fn {_key, ingredients} -> MapSet.size(ingredients) == 1 end)

    case found do
      nil -> resolved
      {allergen, ingredients} ->
        ingredient = Enum.at(MapSet.to_list(ingredients), 0)
        new_resolved = Map.put(resolved, allergen, ingredient)
        new_allergen_map = 
          allergen_map
          |> Map.delete(allergen)
          |> Map.new(fn {key, ingrs} -> {key, MapSet.delete(ingrs, ingredient)} end)

        resolve_allergens(new_allergen_map, new_resolved)
    end
  end
end

AllergenAssessment.run()