
defmodule Day22 do
  def read_input(filename) do
    [map_data, instructions_data] =
      filename
      |> File.read!()
      |> String.split("\n\n", trim: true)

    {parse_map(map_data), parse_instructions(instructions_data)}
  end

  defp parse_map(map_data) do
    map_data
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {line, row_index}, acc ->
      line
      |> String.to_charlist()
      |> Enum.with_index()
      |> Enum.reduce(acc, fn {char, col_index}, row_acc ->
        case char do
          ?. -> Map.put(row_acc, {row_index, col_index}, :open)
          ?# -> Map.put(row_acc, {row_index, col_index}, :wall)
          _ -> row_acc
        end
      end)
    end)
  end

  defp parse_instructions(instructions_data) do
    Regex.scan(~r/(\d+|[RL])/, instructions_data, capture: :all_but_first)
    |> List.flatten()
    |> Enum.map(fn x ->
      case Integer.parse(x) do
        :error -> String.to_atom(x)
        {num, _} -> num
      end
    end)
  end

  defp get_start_position(map) do
    map
    |> Enum.filter(fn {{row, _}, type} -> row == 0 and type == :open end)
    |> Enum.min_by(fn {{_, col}, _} -> col end)
    |> elem(0)
  end

  defp next_position(map, {row, col}, facing) do
    {dr, dc} =
      case facing do
        0 -> {0, 1}
        1 -> {1, 0}
        2 -> {0, -1}
        3 -> {-1, 0}
      end

    next_row = row + dr
    next_col = col + dc

    case Map.get(map, {next_row, next_col}) do
      nil -> wrap_around(map, {row, col}, facing)
      type -> {{next_row, next_col}, type}
    end
  end

  defp wrap_around(map, {row, col}, facing) do
    {dr, dc} =
      case facing do
        0 -> {0, 1}
        1 -> {1, 0}
        2 -> {0, -1}
        3 -> {-1, 0}
      end

    case facing do
      0 ->
        # Right
        map
        |> Enum.filter(fn {{r, _}, _} -> r == row end)
        |> Enum.min_by(fn {{_, c}, _} -> c end)

      1 ->
        # Down
        map
        |> Enum.filter(fn {{_, c}, _} -> c == col end)
        |> Enum.min_by(fn {{r, _}, _} -> r end)

      2 ->
        # Left
        map
        |> Enum.filter(fn {{r, _}, _} -> r == row end)
        |> Enum.max_by(fn {{_, c}, _} -> c end)

      3 ->
        # Up
        map
        |> Enum.filter(fn {{_, c}, _} -> c == col end)
        |> Enum.max_by(fn {{r, _}, _} -> r end)
    end
  end

  defp turn(facing, direction) do
    case direction do
      :R -> (facing + 1) |> rem(4)
      :L -> (facing - 1 + 4) |> rem(4)
    end
  end

  def solve(filename) do
    {map, instructions} = read_input(filename)
    start_position = get_start_position(map)
    initial_facing = 0

    {final_position, final_facing} =
      Enum.reduce(instructions, {start_position, initial_facing}, fn instruction,
                                                                    {current_position, current_facing} ->
        case instruction do
          steps when is_integer(steps) ->
            Enum.reduce(1..steps, {current_position, current_facing}, fn _,
                                                                        {pos, facing} ->
              {next_pos, type} = next_position(map, pos, facing)

              case type do
                :open -> {next_pos, facing}
                :wall -> {pos, facing}
              end
            end)

          direction ->
            {current_position, turn(current_facing, direction)}
        end
      end)

    {row, col} = final_position
    1000 * (row + 1) + 4 * (col + 1) + final_facing
  end
end

Day22.solve("input.txt") |> IO.puts()
