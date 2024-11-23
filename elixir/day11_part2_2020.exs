
defmodule SeatingSimulation do
  @directions [
    {-1, -1}, {0, -1}, {1, -1},
    {-1, 0}, {1, 0},
    {-1, 1}, {0, 1}, {1, 1}
  ]

  def solve do
    seating_area = 
      File.read!("input.txt")
      |> String.split("\n", trim: true)
      |> Enum.map(&String.graphemes/1)

    simulate(seating_area)
    |> count_occupied_seats()
    |> IO.puts()
  end

  defp simulate(seating_area) do
    new_seating_area = 
      for {row, i} <- Enum.with_index(seating_area),
          {seat, j} <- Enum.with_index(row) do
        case seat do
          "L" -> 
            if count_visible_occupied(seating_area, i, j) == 0, do: "#", else: seat
          "#" -> 
            if count_visible_occupied(seating_area, i, j) >= 5, do: "L", else: seat
          _ -> seat
        end
      end
      |> Enum.chunk_every(length(hd(seating_area)))

    if new_seating_area == seating_area do
      new_seating_area
    else
      simulate(new_seating_area)
    end
  end

  defp count_visible_occupied(seating_area, row, col) do
    @directions
    |> Enum.count(fn {dx, dy} ->
      find_first_seat(seating_area, row, col, dx, dy) == "#"
    end)
  end

  defp find_first_seat(seating_area, row, col, dx, dy) do
    new_row = row + dy
    new_col = col + dx

    cond do
      new_row < 0 or new_row >= length(seating_area) -> nil
      new_col < 0 or new_col >= length(hd(seating_area)) -> nil
      true ->
        case Enum.at(Enum.at(seating_area, new_row), new_col) do
          "L" -> "L"
          "#" -> "#"
          "." -> find_first_seat(seating_area, new_row, new_col, dx, dy)
        end
    end
  end

  defp count_occupied_seats(seating_area) do
    seating_area
    |> List.flatten()
    |> Enum.count(&(&1 == "#"))
  end
end

SeatingSimulation.solve()
