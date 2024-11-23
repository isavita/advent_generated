
defmodule SeatingSystem do
  def solve do
    seating_area = 
      File.read!("input.txt")
      |> String.split("\n", trim: true)
      |> Enum.map(&String.graphemes/1)

    stabilize(seating_area)
    |> count_occupied_seats()
    |> IO.puts()
  end

  defp stabilize(seating_area) do
    new_seating_area = simulate_seating(seating_area)
    
    if new_seating_area == seating_area do
      seating_area
    else
      stabilize(new_seating_area)
    end
  end

  defp simulate_seating(seating_area) do
    for {row, i} <- Enum.with_index(seating_area) do
      for {seat, j} <- Enum.with_index(row) do
        update_seat(seating_area, seat, i, j)
      end
    end
  end

  defp update_seat(seating_area, seat, row, col) do
    case seat do
      "L" -> 
        if count_adjacent_occupied(seating_area, row, col) == 0, do: "#", else: "L"
      "#" -> 
        if count_adjacent_occupied(seating_area, row, col) >= 4, do: "L", else: "#"
      _ -> seat
    end
  end

  defp count_adjacent_occupied(seating_area, row, col) do
    for di <- -1..1, dj <- -1..1, 
        di != 0 or dj != 0,
        i = row + di, 
        j = col + dj, 
        i >= 0, 
        i < length(seating_area), 
        j >= 0, 
        j < length(Enum.at(seating_area, 0)) do
      Enum.at(seating_area, i) |> Enum.at(j)
    end
    |> Enum.count(fn seat -> seat == "#" end)
  end

  defp count_occupied_seats(seating_area) do
    seating_area
    |> List.flatten()
    |> Enum.count(fn seat -> seat == "#" end)
  end
end

SeatingSystem.solve()
