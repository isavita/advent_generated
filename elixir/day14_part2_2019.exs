
defmodule Day14 do
  def parse_chemical(s) do
    [amount_str, name] = String.split(s, " ")
    {String.to_integer(amount_str), name}
  end

  def parse_reactions(lines) do
    Enum.reduce(lines, {%{}, %{}}, fn line, {reactions, ingredients} ->
      [inputs_str, output_str] = String.split(line, " => ")
      {output_amount, output_name} = parse_chemical(output_str)

      inputs =
        inputs_str
        |> String.split(", ")
        |> Enum.map(&parse_chemical/1)

      {Map.put(reactions, output_name, {output_amount, output_name}),
       Map.put(ingredients, output_name, inputs)}
    end)
  end

  def calculate_ore(reactions, ingredients, needed, surplus, "ORE", amount) do
    {amount, surplus}
  end

  def calculate_ore(reactions, ingredients, needed, surplus, chem, amount) do
    surplus_amount = Map.get(surplus, chem, 0)

    if surplus_amount >= amount do
      {0, Map.put(surplus, chem, surplus_amount - amount)}
    else
      amount_needed = amount - surplus_amount
      surplus = Map.put(surplus, chem, 0)
      {reaction_amount, _} = Map.get(reactions, chem)

      times =
        :math.ceil(amount_needed / reaction_amount)
        |> trunc()

      ore_needed =
        ingredients
        |> Map.get(chem)
        |> Enum.reduce({0, surplus}, fn {ingredient_amount, ingredient_name}, {acc_ore, acc_surplus} ->
          {ore, new_surplus} =
            calculate_ore(
              reactions,
              ingredients,
              needed,
              acc_surplus,
              ingredient_name,
              ingredient_amount * times
            )

          {acc_ore + ore, new_surplus}
        end)

      surplus =
        Map.put(
          ore_needed |> elem(1),
          chem,
          times * reaction_amount - amount_needed
        )

      {ore_needed |> elem(0), surplus}
    end
  end

  def max_fuel(reactions, ingredients, ore_available) do
    calculate = fn fuel_amount ->
      {ore, _} =
        calculate_ore(reactions, ingredients, %{"FUEL" => fuel_amount}, %{}, "FUEL", fuel_amount)

      ore
    end

    low = 1
    high = ore_available

    binary_search(calculate, low, high, ore_available)
  end

  def binary_search(calculate, low, high, ore_available) do
    if low >= high do
      low
    else
      mid = div(low + high + 1, 2)

      if calculate.(mid) > ore_available do
        binary_search(calculate, low, mid - 1, ore_available)
      else
        binary_search(calculate, mid, high, ore_available)
      end
    end
  end

  def solve() do
    lines =
      "input.txt"
      |> File.read!()
      |> String.split("\n", trim: true)

    {reactions, ingredients} = parse_reactions(lines)
    ore_available = 1_000_000_000_000
    max_fuel(reactions, ingredients, ore_available)
  end
end

IO.puts(Day14.solve())
