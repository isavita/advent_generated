
defmodule Day2 do
  def call do
    input = File.read!("input.txt") |> String.split("\n")
    total_wrapping_paper = Enum.reduce(input, 0, fn line, acc ->
      [l, w, h] = String.split(line, "x") |> Enum.map(&String.to_integer/1)
      side1 = l * w
      side2 = w * h
      side3 = h * l
      slack = min(min(side1, side2), side3)
      2 * side1 + 2 * side2 + 2 * side3 + slack + acc
    end)
    
    total_ribbon = Enum.reduce(input, 0, fn line, acc ->
      [l, w, h] = String.split(line, "x") |> Enum.map(&String.to_integer/1)
      smallest_perimeter = 2 * min(min(l + w, w + h), h + l)
      volume = l * w * h
      smallest_perimeter + volume + acc
    end)
    
    {total_wrapping_paper, total_ribbon}
  end
end
