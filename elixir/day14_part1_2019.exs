
defmodule Chemical do
  def parse(s) do
    [amount, name] = String.split(s)
    %{name: name, amount: String.to_integer(amount)}
  end
end

defmodule Reactions do
  def calculate_ore(chem, amount, reactions, ingredients, surplus) do
    case chem do
      "ORE" ->
        {amount, surplus}

      _ ->
        if Map.get(surplus, chem, 0) >= amount do
          surplus = Map.update!(surplus, chem, &(&1 - amount))
          {0, surplus}
        else
          amount = amount - Map.get(surplus, chem, 0)
          surplus = Map.put(surplus, chem, 0)
          reaction = Map.get(reactions, chem)
          times = div(amount + reaction.amount - 1, reaction.amount)
          {ore, surplus} =
            Enum.reduce(Map.get(ingredients, chem), {0, surplus}, fn ingredient, {acc_ore, acc_surplus} ->
              {ore, new_surplus} = calculate_ore(ingredient.name, ingredient.amount * times, reactions, ingredients, acc_surplus)
              {acc_ore + ore, new_surplus}
            end)

          surplus = Map.update(surplus, chem, times * reaction.amount - amount, fn x -> x + times * reaction.amount - amount end)
          {ore, surplus}
        end
    end
  end
end

defmodule Day14 do
  def main do
    {reactions, ingredients} =
      File.read!("input.txt")
      |> String.split("\n", trim: true)
      |> Enum.reduce({%{}, %{}}, fn line, {acc_reactions, acc_ingredients} ->
        [inputs_str, output_str] = String.split(line, " => ")
        output = Chemical.parse(output_str)
        inputs = String.split(inputs_str, ", ") |> Enum.map(&Chemical.parse/1)
        reactions = Map.put(acc_reactions, output.name, output)
        ingredients = Map.put(acc_ingredients, output.name, inputs)
        {reactions, ingredients}
      end)

    {ore, _} = Reactions.calculate_ore("FUEL", 1, reactions, ingredients, %{})
    IO.puts(ore)
  end
end

Day14.main()
