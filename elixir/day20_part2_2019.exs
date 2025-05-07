
defmodule AoC do
  def read_input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_charlist/1)
  end

  def get_char(grid, x, y) do
    case Enum.fetch(grid, y) do
      {:ok, row} ->
        case Enum.fetch(row, x) do
          {:ok, char_code} -> <<char_code>>
          :error -> nil
        end
      :error -> nil
    end
  end

  def is_letter(char) when is_binary(char) do
    byte_size(char) == 1 and String.first(char) >= "A" and String.first(char) <= "Z"
  end
  def is_letter(_), do: false

  def find_portals(grid) do
    height = Enum.count(grid)
    width = Enum.map(grid, &Enum.count/1) |> Enum.max()

    portal_entries =
      for y <- 0..(height-1), x <- 0..(width-1) do
        char = get_char(grid, x, y)
        if is_letter(char) do
          char_right = get_char(grid, x + 1, y)
          horizontal_entry = if is_letter(char_right) do
            label = char <> char_right
            pos = cond do
              get_char(grid, x - 1, y) == "." -> {x - 1, y}
              get_char(grid, x + 2, y) == "." -> {x + 2, y}
              true -> nil
            end
            if pos, do: [{label, pos}], else: []
          else
            []
          end

          char_down = get_char(grid, x, y + 1)
          vertical_entry = if is_letter(char_down) do
            label = char <> char_down
            pos = cond do
              get_char(grid, x, y - 1) == "." -> {x, y - 1}
              get_char(grid, x, y + 2) == "." -> {x, y + 2}
              true -> nil
            end
            if pos, do: [{label, pos}], else: []
          else
            []
          end

          horizontal_entry ++ vertical_entry
        else
          []
        end
      end
      |> List.flatten()

    portals =
      portal_entries
      |> Enum.group_by(fn {label, _} -> label end, fn {_, pos} -> pos end)
      |> Enum.map(fn {label, positions} -> {label, Enum.uniq(positions)} end)
      |> Map.new()

    {portals, width, height}
  end

  def is_outer({x, y}, width, height) do
    threshold = 3
    x < threshold or y < threshold or x > width - 1 - threshold or y > height - 1 - threshold
  end

  def build_portal_mapping(portals, width, height) do
    start_pos = Map.get(portals, "AA") |> hd()
    end_pos = Map.get(portals, "ZZ") |> hd()

    portal_map =
      portals
      |> Map.reject(fn {label, _} -> label == "AA" or label == "ZZ" end)
      |> Enum.filter(fn {_, positions} -> Enum.count(positions) == 2 end)
      |> Enum.reduce(%{}, fn {_, [pos1, pos2]}, acc_map ->
        pos1_is_outer = is_outer(pos1, width, height)
        pos2_is_outer = is_outer(pos2, width, height)

        acc_map
        |> Map.put(pos1, {pos2, pos1_is_outer})
        |> Map.put(pos2, {pos1, pos2_is_outer})
      end)

    {start_pos, end_pos, portal_map}
  end

  defmodule SimpleQueue do
    def new, do: {[], []}
    def enqueue({in_list, out_list}, item), do: {[item | in_list], out_list}
    def dequeue({[], []}), do: {:empty, {[], []}}
    def dequeue({in_list, []}) do
      out_list = Enum.reverse(in_list)
      dequeue({[], out_list})
    end
    def dequeue({in_list, [h | t]}), do: {:ok, h, {in_list, t}}
    def empty?({[], []}), do: true
    def empty?(_), do: false
  end

  def bfs(grid, start_pos, end_pos, portal_map) do
    initial_state = {elem(start_pos, 0), elem(start_pos, 1), 0, 0}
    queue = SimpleQueue.enqueue(SimpleQueue.new(), initial_state)
    visited = MapSet.new([{elem(start_pos, 0), elem(start_pos, 1), 0}])

    width = Enum.map(grid, &Enum.count/1) |> Enum.max()
    height = Enum.count(grid)

    directions = [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]

    bfs_loop(queue, visited, grid, end_pos, portal_map, width, height, directions)
  end

  def bfs_loop(queue, visited, grid, end_pos, portal_map, width, height, directions) do
    case SimpleQueue.dequeue(queue) do
      {:empty, _} ->
        -1

      {:ok, {x, y, level, steps}, rest_queue} ->
        if {x, y} == end_pos and level == 0 do
          steps
        else
          potential_next_states =
            Enum.filter_map(directions, fn {dx, dy} ->
              nx = x + dx
              ny = y + dy
              get_char(grid, nx, ny) == "."
            end, fn {dx, dy} ->
              nx = x + dx
              ny = y + dy
              {nx, ny, level, steps + 1}
            end)
            |> then(fn neighbor_states ->
              case Map.get(portal_map, {x, y}) do
                nil -> neighbor_states
                {target_pos, is_outer} ->
                  new_level = if is_outer, do: level - 1, else: level + 1
                  if new_level >= 0 do
                    tx = elem(target_pos, 0)
                    ty = elem(target_pos, 1)
                    [{tx, ty, new_level, steps + 1} | neighbor_states]
                  else
                    neighbor_states
                  end
              end
            end)

          {next_queue, next_visited} =
            Enum.reduce(potential_next_states, {rest_queue, visited}, fn {nx, ny, next_level, next_steps}, {current_queue, current_visited} ->
              state_key = {nx, ny, next_level}
              if not MapSet.member?(current_visited, state_key) do
                new_visited = MapSet.put(current_visited, state_key)
                new_queue = SimpleQueue.enqueue(current_queue, {nx, ny, next_level, next_steps})
                {new_queue, new_visited}
              else
                {current_queue, current_visited}
              end
            end)

          bfs_loop(next_queue, next_visited, grid, end_pos, portal_map, width, height, directions)
        end
    end
  end

  def main() do
    grid = read_input("input.txt")
    {portals, width, height} = find_portals(grid)
    {start_pos, end_pos, portal_map} = build_portal_mapping(portals, width, height)
    result = bfs(grid, start_pos, end_pos, portal_map)
    IO.puts(result)
  end
end

AoC.main()
