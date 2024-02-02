
defmodule MarbleMania do
  def call do
    {players, last_marble} = File.read!("input.txt") |> parse_input
    scores = play_game(players, last_marble)
    Enum.max(scores)
  end

  defp parse_input(input) do
    [players, last_marble] = Regex.scan(~r/\d+/, input) |> List.flatten() |> Enum.map(&String.to_integer/1)
    {players, last_marble}
  end

  defp play_game(players, last_marble) do
    1..last_marble
    |> Enum.reduce({[0], %{}, 0}, fn marble, {circle, scores, current_index} ->
      if rem(marble, 23) == 0 do
        {new_circle, removed_marble, new_current_index} = remove_marble(circle, current_index)
        new_scores = Map.update(scores, rem(marble, players), marble + removed_marble, &(&1 + marble + removed_marble))
        {new_circle, new_scores, new_current_index}
      else
        {new_circle, new_current_index} = insert_marble(circle, marble, current_index)
        {new_circle, scores, new_current_index}
      end
    end)
    |> elem(1)
    |> Map.values()
  end

  defp insert_marble(circle, marble, current_index) do
    new_index = rem(current_index + 2, length(circle))
    new_circle = List.insert_at(circle, new_index, marble)
    {new_circle, new_index}
  end

  defp remove_marble(circle, current_index) do
    remove_index = rem(current_index - 7 + length(circle), length(circle))
    {removed_marble, new_circle} = List.pop_at(circle, remove_index)
    {new_circle, removed_marble, remove_index}
  end
end
