
defmodule AuntSue do
  def call do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
    |> Enum.find(&match_gift?/1)
    |> elem(0)
  end

  defp parse_line(line) do
    [_, num, traits] = Regex.run(~r/Sue (\d+): (.+)/, line)
    {String.to_integer(num), parse_traits(traits)}
  end

  defp parse_traits(traits) do
    traits
    |> String.split(", ")
    |> Enum.map(fn trait ->
      [name, count] = String.split(trait, ": ")
      {String.to_atom(name), String.to_integer(count)}
    end)
    |> Enum.into(%{})
  end

  defp match_gift?(sue) do
    gift_traits = %{
      children: 3,
      cats: 7,
      samoyeds: 2,
      pomeranians: 3,
      akitas: 0,
      vizslas: 0,
      goldfish: 5,
      trees: 3,
      cars: 2,
      perfumes: 1
    }

    Enum.all?(gift_traits, fn {trait, count} ->
      Map.get(sue |> elem(1), trait, count) == count
    end)
  end
end
