
defmodule Day23 do
  def read_input(filename) do
    filename
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.reduce(MapSet.new(), fn {line, row}, acc ->
      line
      |> String.to_charlist()
      |> Enum.with_index()
      |> Enum.reduce(acc, fn {char, col}, acc2 ->
        if char == ?#, do: MapSet.put(acc2, {row, col}), else: acc2
      end)
    end)
  end

  def propose_moves(elves, directions) do
    elves
    |> Enum.reduce(%{}, fn {r, c} = elf, acc ->
      neighbors = [
        {r - 1, c - 1},
        {r - 1, c},
        {r - 1, c + 1},
        {r, c - 1},
        {r, c + 1},
        {r + 1, c - 1},
        {r + 1, c},
        {r + 1, c + 1}
      ]

      if Enum.all?(neighbors, &(!MapSet.member?(elves, &1))) do
        acc
      else
        proposed =
          Enum.find_value(directions, fn dir ->
            case dir do
              :N ->
                if Enum.all?([
                     {r - 1, c - 1},
                     {r - 1, c},
                     {r - 1, c + 1}
                   ], &(!MapSet.member?(elves, &1))) do
                  {r - 1, c}
                end

              :S ->
                if Enum.all?([
                     {r + 1, c - 1},
                     {r + 1, c},
                     {r + 1, c + 1}
                   ], &(!MapSet.member?(elves, &1))) do
                  {r + 1, c}
                end

              :W ->
                if Enum.all?([
                     {r - 1, c - 1},
                     {r, c - 1},
                     {r + 1, c - 1}
                   ], &(!MapSet.member?(elves, &1))) do
                  {r, c - 1}
                end

              :E ->
                if Enum.all?([
                     {r - 1, c + 1},
                     {r, c + 1},
                     {r + 1, c + 1}
                   ], &(!MapSet.member?(elves, &1))) do
                  {r, c + 1}
                end
            end
          end)

        if proposed do
          Map.update(acc, proposed, [elf], &[elf | &1])
        else
          acc
        end
      end
    end)
  end

  def move_elves(elves, proposals) do
    proposals
    |> Enum.reduce(elves, fn {proposed_pos, elves_list}, acc ->
      if length(elves_list) == 1 do
        [original_pos] = elves_list
        acc |> MapSet.delete(original_pos) |> MapSet.put(proposed_pos)
      else
        acc
      end
    end)
  end

  def simulate_rounds(elves, rounds) do
    directions = [:N, :S, :W, :E]

    do_simulate(elves, directions, rounds)
  end

  defp do_simulate(elves, directions, 0), do: elves

  defp do_simulate(elves, directions, rounds) do
    proposals = propose_moves(elves, directions)
    new_elves = move_elves(elves, proposals)
    new_directions = tl(directions) ++ [hd(directions)]
    do_simulate(new_elves, new_directions, rounds - 1)
  end

  def bounding_box(elves) do
    rows = Enum.map(elves, fn {r, _} -> r end)
    cols = Enum.map(elves, fn {_, c} -> c end)

    {min_row, max_row} = {Enum.min(rows), Enum.max(rows)}
    {min_col, max_col} = {Enum.min(cols), Enum.max(cols)}

    {min_row, max_row, min_col, max_col}
  end

  def count_empty_tiles(elves) do
    {min_row, max_row, min_col, max_col} = bounding_box(elves)

    area = (max_row - min_row + 1) * (max_col - min_col + 1)
    area - MapSet.size(elves)
  end

  def part1(filename) do
    elves = read_input(filename)
    final_elves = simulate_rounds(elves, 10)
    count_empty_tiles(final_elves)
  end
end

Day23.part1("input.txt") |> IO.puts()
