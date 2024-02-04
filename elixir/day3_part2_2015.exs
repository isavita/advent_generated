
defmodule SantaDelivery do
  def call do
    "input.txt"
    |> File.read!()
    |> calculate_houses()
  end

  defp calculate_houses(input) do
    input
    |> String.trim()
    |> String.graphemes()
    |> Enum.chunk_every(2)
    |> Enum.reduce({%{x: 0, y: 0}, %{x: 0, y: 0}, %{}, 1}, fn
      [santa_move, robo_santa_move], {santa_pos, robo_pos, houses, turn} ->
        santa_new_pos = move(santa_pos, santa_move)
        robo_new_pos = move(robo_pos, robo_santa_move)
        new_houses = Map.put(houses, santa_new_pos, true) |> Map.put(robo_new_pos, true)
        {santa_new_pos, robo_new_pos, new_houses, turn * -1}
    end)
    |> elem(2)
    |> Map.size()
  end

  defp move(pos, "^"), do: %{pos | y: pos.y + 1}
  defp move(pos, "v"), do: %{pos | y: pos.y - 1}
  defp move(pos, ">"), do: %{pos | x: pos.x + 1}
  defp move(pos, "<"), do: %{pos | x: pos.x - 1}
end

SantaDelivery.call() |> IO.puts()
