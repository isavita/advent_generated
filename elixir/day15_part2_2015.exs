defmodule Ingredient do
  defstruct name: "", capacity: 0, durability: 0, flavor: 0, texture: 0, calories: 0
end

defmodule Solution do
  def readIngredients(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(&parseIngredient/1)
  end

  defp parseIngredient(line) do
    [name, capacity, durability, flavor, texture, calories] =
      Regex.run(~r/(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)/, line, [capture: :all_but_first, trim: true])

    %Ingredient{
      name: name,
      capacity: String.to_integer(capacity),
      durability: String.to_integer(durability),
      flavor: String.to_integer(flavor),
      texture: String.to_integer(texture),
      calories: String.to_integer(calories)
    }
  end

  def findMaxScore(ingredients, totalTeaspoons, targetCalories) do
    calculateMaxScore(ingredients, 0, totalTeaspoons, [], targetCalories)
  end

  defp calculateMaxScore(ingredients, index, remaining, teaspoons, targetCalories) do
    if index == length(ingredients) - 1 do
      teaspoons = teaspoons ++ [remaining]
      if calculateCalories(ingredients, teaspoons) == targetCalories do
        score(ingredients, teaspoons)
      else
        0
      end
    else
      Enum.reduce(0..remaining, 0, fn i, maxScore ->
        score = calculateMaxScore(ingredients, index + 1, remaining - i, teaspoons ++ [i], targetCalories)
        max(score, maxScore)
      end)
    end
  end

  defp score(ingredients, teaspoons) do
    {capacity, durability, flavor, texture} =
      Enum.zip(ingredients, teaspoons)
      |> Enum.reduce({0, 0, 0, 0}, fn {ingredient, teaspoon}, {c, d, f, t} ->
        {c + ingredient.capacity * teaspoon, d + ingredient.durability * teaspoon, f + ingredient.flavor * teaspoon, t + ingredient.texture * teaspoon}
      end)

    capacity * durability * flavor * texture
  end

  defp calculateCalories(ingredients, teaspoons) do
    Enum.zip(ingredients, teaspoons)
    |> Enum.map(fn {ingredient, teaspoon} -> ingredient.calories * teaspoon end)
    |> Enum.sum()
  end
end

ingredients = Solution.readIngredients("input.txt")
maxScore = Solution.findMaxScore(ingredients, 100, 500)
IO.puts(maxScore)
