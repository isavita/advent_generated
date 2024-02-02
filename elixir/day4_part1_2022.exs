
defmodule CampCleanup do
  def call do
    input = File.read!("input.txt")
    pairs = String.split(input, "\n")

    count = Enum.count(pairs, fn pair ->
      [range1, range2] = String.split(pair, ",")
      [start1, end1] = String.split(range1, "-") |> Enum.map(&String.to_integer/1)
      [start2, end2] = String.split(range2, "-") |> Enum.map(&String.to_integer/1)

      (start1 <= start2 && end1 >= end2) || (start2 <= start1 && end2 >= end1)
    end)

    count
  end
end
