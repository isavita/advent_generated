input = File.read!("input.txt")
lines = String.split(input, "\n", trim: true)

slopes = [
  [1, 1],
  [3, 1],
  [5, 1],
  [7, 1],
  [1, 2]
]

product = Enum.reduce(slopes, 1, fn [right, down], acc ->
  {_, treeCount} = Enum.reduce(0..(length(lines)-1), {0, 0}, fn i, {pos, acc} ->
    cond do
      rem(i, down) != 0 -> {pos, acc} # Skip if not on the correct row
      String.at(Enum.at(lines, i), rem(pos, String.length(Enum.at(lines, i)))) == "#" -> {pos + right, acc + 1} # Hit a tree
      true -> {pos + right, acc} # Did not hit a tree
    end
  end)
  acc * treeCount
end)

IO.puts product