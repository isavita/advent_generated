
defmodule Day5 do
  def call do
    input = File.read!("input.txt") |> String.split("\n", trim: true)
    seat_ids = input |> Enum.map(&calculate_seat_id/1)
    max_seat_id = Enum.max(seat_ids)
    missing_seat_id = Enum.reduce(0..max_seat_id, nil, fn x, acc ->
      if !Enum.member?(seat_ids, x) && Enum.member?(seat_ids, x - 1) && Enum.member?(seat_ids, x + 1) do
        x
      else
        acc
      end
    end)
    missing_seat_id
  end

  defp calculate_seat_id(boarding_pass) do
    row = boarding_pass |> String.slice(0..6) |> String.replace("F", "0") |> String.replace("B", "1") |> Integer.parse(2) |> elem(0)
    column = boarding_pass |> String.slice(7..9) |> String.replace("L", "0") |> String.replace("R", "1") |> Integer.parse(2) |> elem(0)
    row * 8 + column
  end
end
