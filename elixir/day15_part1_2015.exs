defmodule CookieRecipe do
  def call do
    # Read input from file
    input = read_input()

    # Parse ingredients from input
    ingredients = parse_ingredients(input)

    # Find the highest-scoring cookie
    highest_score = find_highest_score(ingredients)

    # Print the result
    IO.puts(highest_score)
  end

  defp read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
  end

  defp parse_ingredients(input) do
    Enum.map(input, fn line ->
      [name, properties] = String.split(line, ": ")
      properties = String.split(properties, ", ")

      %{
        name: name,
        capacity: extract_property(properties, "capacity"),
        durability: extract_property(properties, "durability"),
        flavor: extract_property(properties, "flavor"),
        texture: extract_property(properties, "texture"),
        calories: extract_property(properties, "calories")
      }
    end)
  end

  defp extract_property(properties, property_name) do
    Enum.find_value(properties, fn property ->
      [name, value] = String.split(property, " ")
      if name == property_name, do: String.to_integer(value)
    end)
  end

  defp find_highest_score(ingredients) do
    range = 0..100
    combinations = for i <- range,
                       j <- range,
                       k <- range,
                       l <- range,
                       i + j + k + l == 100,
                       do: [i, j, k, l]

    Enum.reduce(combinations, 0, fn amounts, highest_score ->
      score = calculate_score(ingredients, amounts)
      max(score, highest_score)
    end)
  end

  defp calculate_score(ingredients, amounts) do
    properties = %{capacity: 0, durability: 0, flavor: 0, texture: 0}

    properties = Enum.zip(ingredients, amounts)
                 |> Enum.reduce(properties, fn {ingredient, amount}, acc ->
                   %{
                     capacity: acc.capacity + ingredient.capacity * amount,
                     durability: acc.durability + ingredient.durability * amount,
                     flavor: acc.flavor + ingredient.flavor * amount,
                     texture: acc.texture + ingredient.texture * amount
                   }
                 end)

    Enum.reduce(properties, 1, fn {_, value}, score ->
      score * max(value, 0)
    end)
  end
end

CookieRecipe.call()
