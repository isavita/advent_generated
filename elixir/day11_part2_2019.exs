
defmodule VM do
  def start(program, parent) do
    spawn(fn -> loop(%{code: program, ip: 0, rel: 0, parent: parent}) end)
  end

  defp loop(state) do
    ip = state.ip
    instr = Map.get(state.code, ip, 0)
    opcode = rem(instr, 100)
    modes = div(instr, 100)

    case opcode do
      1 -> bin_op(&+/2, state, modes, 3)
      2 -> bin_op(&*/2, state, modes, 3)
      3 ->
        receive do
          {:input, v} ->
            addr = param_addr(state, ip + 1, mode(modes, 0))
            code = Map.put(state.code, addr, v)
            loop(%{state | code: code, ip: ip + 2})
        end

      4 ->
        addr = param_addr(state, ip + 1, mode(modes, 0))
        send(state.parent, {:output, Map.get(state.code, addr, 0)})
        loop(%{state | ip: ip + 2})

      5 -> jump(state, modes, fn x -> x != 0 end, 2)
      6 -> jump(state, modes, fn x -> x == 0 end, 2)

      7 ->
        bin_op(fn a, b -> if a < b, do: 1, else: 0 end, state, modes, 3)

      8 ->
        bin_op(fn a, b -> if a == b, do: 1, else: 0 end, state, modes, 3)

      9 ->
        addr = param_addr(state, ip + 1, mode(modes, 0))
        rel = state.rel + Map.get(state.code, addr, 0)
        loop(%{state | rel: rel, ip: ip + 2})

      99 ->
        send(state.parent, :halt)

      _ -> :ok
    end
  end

  defp mode(modes, i), do: rem(div(modes, :math.pow(10, i) |> trunc), 10)

  defp param_addr(state, pos, 0), do: Map.get(state.code, pos, 0)
  defp param_addr(_state, pos, 1), do: pos
  defp param_addr(state, pos, 2), do: state.rel + Map.get(state.code, pos, 0)

  defp get(state, pos, m), do: Map.get(state.code, param_addr(state, pos, m), 0)

  defp bin_op(fun, state, modes, ar) do
    a = get(state, state.ip + 1, mode(modes, 0))
    b = get(state, state.ip + 2, mode(modes, 1))
    dst = param_addr(state, state.ip + 3, mode(modes, 2))
    code = Map.put(state.code, dst, fun.(a, b))
    loop(%{state | code: code, ip: state.ip + ar + 1})
  end

  defp jump(state, modes, cond, ar) do
    test = get(state, state.ip + 1, mode(modes, 0))
    target = get(state, state.ip + 2, mode(modes, 1))

    if cond.(test), do: loop(%{state | ip: target}), else: loop(%{state | ip: state.ip + ar + 1})
  end
end

defmodule Robot do
  def start(grid) do
    parent = self()
    vm = VM.start(load_program(), parent)
    loop(vm, %{pos: {0, 0}, dir: 0}, grid)
  end

  defp load_program do
    "input.txt"
    |> File.read!()
    |> String.trim()
    |> String.split(",", trim: true)
    |> Enum.map(&String.to_integer/1)
    |> Enum.with_index()
    |> Enum.into(%{}, fn {v, i} -> {i, v} end)
  end

  defp loop(vm, robot, grid) do
    send(vm, {:input, Map.get(grid, robot.pos, 0)})

    receive do
      {:output, color} ->
        receive do
          {:output, turn} ->
            grid = Map.put(grid, robot.pos, color)
            robot = turn(robot, turn) |> move()
            loop(vm, robot, grid)

          :halt -> render(grid)
        end

      :halt -> render(grid)
    end
  end

  defp turn(%{dir: d} = r, 0), do: %{r | dir: rem(d + 3, 4)}
  defp turn(%{dir: d} = r, 1), do: %{r | dir: rem(d + 1, 4)}

  defp move(%{pos: {x, y}, dir: 0} = r), do: %{r | pos: {x, y + 1}}
  defp move(%{pos: {x, y}, dir: 1} = r), do: %{r | pos: {x + 1, y}}
  defp move(%{pos: {x, y}, dir: 2} = r), do: %{r | pos: {x, y - 1}}
  defp move(%{pos: {x, y}, dir: 3} = r), do: %{r | pos: {x - 1, y}}

  defp render(grid) do
    for j <- Enum.reverse(-5..1) do
      for i <- 0..40 do
        case Map.get(grid, {i, j}, 0) do
          0 -> IO.write(" ")
          1 -> IO.write("█")
        end
      end

      IO.puts("")
    end
  end
end

defmodule Main do
  def main, do: Robot.start(%{{0, 0} => 1})
end

Main.main()
