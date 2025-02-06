
defmodule Solver do
  def solve(input) do
    starting_elves = String.to_integer(input)
    find_winner(starting_elves)
  end

  def find_winner(n) do
    highest_power_of_3 = :math.pow(3, floor(:math.log(n) / :math.log(3))) |> round()
    l = n - highest_power_of_3

    if n == highest_power_of_3 do
      n
    else
      if l <= highest_power_of_3 do
        l
      else
        highest_power_of_3 + 2 * (l - highest_power_of_3)
      end
    end
  end
end

input = File.read!("input.txt") |> String.trim_trailing()
Solver.solve(input) |> IO.puts()
