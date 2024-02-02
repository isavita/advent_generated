
defmodule TobogganTrajectory do
  def call do
    input = File.read!("input.txt")
    map = String.split(input, "\n", trim: true)

    count_trees(map, 3, 1)
  end

  defp count_trees(map, right, down) do
    {_, trees} = Enum.reduce(0..length(map) - 1, {0, 0}, fn i, {x, trees} ->
      if rem(i, down) == 0 do
        new_x = rem(x + right, String.length(Enum.at(map, i)))
        new_trees = if String.at(Enum.at(map, i), x) == "#", do: trees + 1, else: trees
        {new_x, new_trees}
      else
        {x, trees}
      end
    end)

    trees
  end
end
