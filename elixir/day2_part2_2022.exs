defmodule Day2 do
  def call() do
    "input.txt"
    |> read_input
    |> Enum.map(&parse_round/1)
    |> Enum.map(&score_round/1)
    |> Enum.sum()
    |> IO.puts()
  end

  defp read_input(file_path) do
    File.read!(file_path)
    |> String.trim()
    |> String.split("\n", trim: true)
  end

  defp parse_round(line) do
    [opponent, outcome] = String.split(line, " ", trim: true)
    {opponent, outcome}
  end

  defp score_round({opponent, outcome}) do
    shape_score = shape_to_play(opponent, outcome) |> score_shape
    outcome_score = score_outcome(outcome)
    shape_score + outcome_score
  end

  defp shape_to_play(opponent, "X"), do: lose_to(opponent)
  defp shape_to_play(opponent, "Y"), do: opponent
  defp shape_to_play(opponent, "Z"), do: win_against(opponent)

  defp lose_to("A"), do: "C"  # Rock loses to Paper
  defp lose_to("B"), do: "A"  # Paper loses to Scissors
  defp lose_to("C"), do: "B"  # Scissors loses to Rock

  defp win_against("A"), do: "B"  # Rock is defeated by Paper
  defp win_against("B"), do: "C"  # Paper is defeated by Scissors
  defp win_against("C"), do: "A"  # Scissors is defeated by Rock

  defp score_shape("A"), do: 1  # Rock
  defp score_shape("B"), do: 2  # Paper
  defp score_shape("C"), do: 3  # Scissors

  defp score_outcome("X"), do: 0  # Lose
  defp score_outcome("Y"), do: 3  # Draw
  defp score_outcome("Z"), do: 6  # Win
end

Day2.call()
