
defmodule Solution do
  def main do
    program = File.read!("input.txt")
              |> String.trim()
              |> String.split(",")
              |> Enum.map(&String.to_integer/1)
              |> Enum.with_index()
              |> Enum.into(%{}, fn {v, i} -> {i, v} end)

    outputs1 = run(program, 0, 0, [], [])
    grid_map = outputs1 |> Enum.reduce({{0, 0}, %{}}, fn
      10, {{_, y}, map} -> {{0, y + 1}, map}
      char, {{x, y}, map} -> {{x + 1, y}, Map.put(map, {x, y}, char)}
    end) |> elem(1)

    intersections = Enum.filter(grid_map, fn {{x, y}, char} ->
      char == ?# and grid_map[{x-1, y}] == ?# and grid_map[{x+1, y}] == ?# and
      grid_map[{x, y-1}] == ?# and grid_map[{x, y+1}] == ?#
    end)
    p1 = Enum.map(intersections, fn {{x, y}, _} -> x * y end) |> Enum.sum()
    IO.puts "Part One: Sum of alignment parameters = #{p1}"

    {{rx, ry}, dir} = Enum.find(grid_map, fn {_, v} -> v in [?^, ?v, ?<, ?>] end)
    path = get_path(grid_map, rx, ry, dir, 0, [])
    {patterns, main_routine} = solve_comp(path, %{}, [])
    p_map = Map.new(Enum.map(patterns, fn {k, v} -> {v, k} end))
    
    all_inputs = [main_routine, p_map["A"], p_map["B"], p_map["C"], ["n"]]
                 |> Enum.map(&Enum.join(&1, ","))
                 |> Enum.join("\n")
                 |> Kernel.<>("\n")
                 |> String.to_charlist()

    p2 = run(Map.put(program, 0, 2), 0, 0, all_inputs, []) |> List.last()
    IO.puts "Part Two: Dust collected = #{p2}"
  end

  defp get_v(mem, pc, mode, rb) do
    v = Map.get(mem, pc, 0)
    case mode do
      0 -> Map.get(mem, v, 0)
      1 -> v
      2 -> Map.get(mem, rb + v, 0)
    end
  end

  defp get_a(mem, pc, mode, rb) do
    v = Map.get(mem, pc, 0)
    if mode == 2, do: rb + v, else: v
  end

  defp run(mem, pc, rb, inputs, outputs) do
    instr = Map.get(mem, pc, 0)
    op = rem(instr, 100)
    [m1, m2, m3] = [div(rem(instr, 1000), 100), div(rem(instr, 10000), 1000), div(rem(instr, 100000), 10000)]
    case op do
      1 -> run(Map.put(mem, get_a(mem, pc+3, m3, rb), get_v(mem, pc+1, m1, rb) + get_v(mem, pc+2, m2, rb)), pc+4, rb, inputs, outputs)
      2 -> run(Map.put(mem, get_a(mem, pc+3, m3, rb), get_v(mem, pc+1, m1, rb) * get_v(mem, pc+2, m2, rb)), pc+4, rb, inputs, outputs)
      3 -> [h | t] = inputs
           run(Map.put(mem, get_a(mem, pc+1, m1, rb), h), pc+2, rb, t, outputs)
      4 -> run(mem, pc+2, rb, inputs, [get_v(mem, pc+1, m1, rb) | outputs])
      5 -> next = if get_v(mem, pc+1, m1, rb) != 0, do: get_v(mem, pc+2, m2, rb), else: pc+3
           run(mem, next, rb, inputs, outputs)
      6 -> next = if get_v(mem, pc+1, m1, rb) == 0, do: get_v(mem, pc+2, m2, rb), else: pc+3
           run(mem, next, rb, inputs, outputs)
      7 -> v = if get_v(mem, pc+1, m1, rb) < get_v(mem, pc+2, m2, rb), do: 1, else: 0
           run(Map.put(mem, get_a(mem, pc+3, m3, rb), v), pc+4, rb, inputs, outputs)
      8 -> v = if get_v(mem, pc+1, m1, rb) == get_v(mem, pc+2, m2, rb), do: 1, else: 0
           run(Map.put(mem, get_a(mem, pc+3, m3, rb), v), pc+4, rb, inputs, outputs)
      9 -> run(mem, pc+2, rb + get_v(mem, pc+1, m1, rb), inputs, outputs)
      99 -> Enum.reverse(outputs)
    end
  end

  defp get_path(grid, x, y, dir, steps, path) do
    {dx, dy} = %{?^ => {0, -1}, ?v => {0, 1}, ?< => {-1, 0}, ?> => {1, 0}}[dir]
    {nx, ny} = {x + dx, y + dy}
    if Map.get(grid, {nx, ny}) == ?# do
      get_path(grid, nx, ny, dir, steps + 1, path)
    else
      new_path = if steps > 0, do: path ++ [to_string(steps)], else: path
      {ldir, rdir} = case dir do ?^ -> {?<, ?>}; ?v -> {?>, ?<}; ?< -> {?v, ?^}; ?> -> {?^, ?v} end
      {lx, ly} = case ldir do ?^ -> {x, y-1}; ?v -> {x, y+1}; ?< -> {x-1, y}; ?> -> {x+1, y} end
      {rx, ry} = case rdir do ?^ -> {x, y-1}; ?v -> {x, y+1}; ?< -> {x-1, y}; ?> -> {x+1, y} end
      cond do
        Map.get(grid, {lx, ly}) == ?# -> get_path(grid, x, y, ldir, 0, new_path ++ ["L"])
        Map.get(grid, {rx, ry}) == ?# -> get_path(grid, x, y, rdir, 0, new_path ++ ["R"])
        true -> new_path
      end
    end
  end

  defp solve_comp(tokens, patterns, main) do
    cond do
      tokens == [] -> if String.length(Enum.join(main, ",")) <= 20, do: {patterns, main}, else: nil
      true ->
        Enum.find_value(patterns |> Map.to_list() |> Enum.sort_by(&elem(&1, 1)), fn {p, name} ->
          if List.starts_with?(tokens, p), do: solve_comp(Enum.drop(tokens, length(p)), patterns, main ++ [name])
        end) || (if map_size(patterns) < 3 do
          Enum.find_value(1..min(length(tokens), 10), fn len ->
            p = Enum.take(tokens, len)
            if String.length(Enum.join(p, ",")) <= 20 do
              name = Enum.at(["A", "B", "C"], map_size(patterns))
              solve_comp(Enum.drop(tokens, len), Map.put(patterns, p, name), main ++ [name])
            end
          end)
        end)
    end
  end
end

Solution.main()
