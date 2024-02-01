
defmodule Day5 do
  def call do
    input = File.read!("input.txt") |> String.split("\n", trim: true)
    seat_ids = input |> Enum.map(&calculate_seat_id/1)
    Enum.max(seat_ids)
  end

  defp calculate_seat_id(boarding_pass) do
    row = boarding_pass |> String.slice(0..6) |> String.replace("F", "0") |> String.replace("B", "1") |> Integer.parse(2) |> elem(0)
    column = boarding_pass |> String.slice(7..9) |> String.replace("L", "0") |> String.replace("R", "1") |> Integer.parse(2) |> elem(0)
    row * 8 + column
  end
end
