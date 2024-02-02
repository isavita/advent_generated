
defmodule CampCleanup do
  def call do
    input = File.read!("input.txt") |> String.trim() |> String.split("\n")

    fully_contained_count = Enum.count(input, fn line ->
      [range1, range2] = String.split(line, ",")
      [start1, end1] = String.split(range1, "-") |> Enum.map(&String.to_integer/1)
      [start2, end2] = String.split(range2, "-") |> Enum.map(&String.to_integer/1)

      (start1 <= start2 && end1 >= end2) || (start1 >= start2 && end1 <= end2)
    end)

    overlaps_count = Enum.count(input, fn line ->
      [range1, range2] = String.split(line, ",")
      [start1, end1] = String.split(range1, "-") |> Enum.map(&String.to_integer/1)
      [start2, end2] = String.split(range2, "-") |> Enum.map(&String.to_integer/1)

      start1 <= end2 && start2 <= end1
    end)

    overlaps_count
  end
end
