defmodule Day2 do
  def call() do
    "input.txt"
    |> read_input
    |> Enum.map(&parse_round/1)
    |> Enum.map(&calculate_score/1)
    |> Enum.sum()
    |> IO.puts()
  end

  defp read_input(file_path) do
    File.read!(file_path)
    |> String.trim()
    |> String.split("\n", trim: true)
  end

  defp parse_round(line) do
    [opponent, your_choice] = String.split(line, " ", trim: true)
    {opponent, your_choice}
  end

  defp calculate_score({opponent, your_choice}) do
    score_map = %{"A" => 1, "B" => 2, "C" => 3, "X" => 1, "Y" => 2, "Z" => 3}
    outcome_score = case {opponent, your_choice} do
      {"A", "Y"} -> 6 # Rock loses to Paper
      {"B", "Z"} -> 6 # Paper loses to Scissors
      {"C", "X"} -> 6 # Scissors loses to Rock
      {"A", "Z"} -> 0 # Rock beats Scissors
      {"B", "X"} -> 0 # Paper beats Rock
      {"C", "Y"} -> 0 # Scissors beats Paper
      _ -> 3 # Draw
    end
    score_map[your_choice] + outcome_score
  end
end

Day2.call()
