
defmodule ElfGame do
  def main do
    total_elves = read_input("input.txt")
    winner = find_winning_elf(total_elves)
    IO.puts(winner)
  end

  defp read_input(filename) do
    {total_elves, _} = File.read!(filename) |> String.trim() |> Integer.parse()
    total_elves
  end

  defp find_winning_elf(total_elves) do
    highest_power_of_two = :math.pow(2, :math.log2(total_elves) |> floor()) |> round()
    (total_elves - highest_power_of_two) * 2 + 1
  end
end

ElfGame.main()
