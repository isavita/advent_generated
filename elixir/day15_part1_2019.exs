
defmodule IntcodeComputer do
  def new(program) do
    %{
      data: program |> Enum.with_index() |> Map.new(fn {v, k} -> {k, v} end),
      ip: 0,
      relbase: 0
    }
  end

  def run(machine, inputs) do
    do_run(machine, inputs, [])
  end

  defp do_run(machine, inputs, outputs) do
    case step(machine, inputs) do
      {:output, output, new_machine, new_inputs} -> 
        do_run(new_machine, new_inputs, [output | outputs])
      {:halt, _machine} -> 
        Enum.reverse(outputs)
      {:need_input, new_machine} -> 
        {:need_input, new_machine, Enum.reverse(outputs)}
      {:continue, new_machine, new_inputs} -> 
        do_run(new_machine, new_inputs, outputs)
    end
  end

  defp step(machine, inputs) do
    %{data: data, ip: ip, relbase: relbase} = machine
    {op, modes} = decode(Map.get(data, ip, 0))

    case op do
      1 -> # Add
        val = get_param(data, ip + 1, Enum.at(modes, 0), relbase) + 
              get_param(data, ip + 2, Enum.at(modes, 1), relbase)
        new_data = set_param(data, ip + 3, Enum.at(modes, 2), relbase, val)
        {:continue, %{machine | data: new_data, ip: ip + 4}, inputs}

      2 -> # Multiply
        val = get_param(data, ip + 1, Enum.at(modes, 0), relbase) * 
              get_param(data, ip + 2, Enum.at(modes, 1), relbase)
        new_data = set_param(data, ip + 3, Enum.at(modes, 2), relbase, val)
        {:continue, %{machine | data: new_data, ip: ip + 4}, inputs}

      3 -> # Input
        case inputs do
          [input | rest] ->
            new_data = set_param(data, ip + 1, Enum.at(modes, 0), relbase, input)
            {:continue, %{machine | data: new_data, ip: ip + 2}, rest}
          [] -> 
            {:need_input, machine}
        end

      4 -> # Output
        output = get_param(data, ip + 1, Enum.at(modes, 0), relbase)
        {:output, output, %{machine | ip: ip + 2}, inputs}

      5 -> # Jump if true
        if get_param(data, ip + 1, Enum.at(modes, 0), relbase) != 0 do
          new_ip = get_param(data, ip + 2, Enum.at(modes, 1), relbase)
          {:continue, %{machine | ip: new_ip}, inputs}
        else
          {:continue, %{machine | ip: ip + 3}, inputs}
        end

      6 -> # Jump if false
        if get_param(data, ip + 1, Enum.at(modes, 0), relbase) == 0 do
          new_ip = get_param(data, ip + 2, Enum.at(modes, 1), relbase)
          {:continue, %{machine | ip: new_ip}, inputs}
        else
          {:continue, %{machine | ip: ip + 3}, inputs}
        end

      7 -> # Less than
        val = if get_param(data, ip + 1, Enum.at(modes, 0), relbase) < 
                 get_param(data, ip + 2, Enum.at(modes, 1), relbase), do: 1, else: 0
        new_data = set_param(data, ip + 3, Enum.at(modes, 2), relbase, val)
        {:continue, %{machine | data: new_data, ip: ip + 4}, inputs}

      8 -> # Equals
        val = if get_param(data, ip + 1, Enum.at(modes, 0), relbase) == 
                 get_param(data, ip + 2, Enum.at(modes, 1), relbase), do: 1, else: 0
        new_data = set_param(data, ip + 3, Enum.at(modes, 2), relbase, val)
        {:continue, %{machine | data: new_data, ip: ip + 4}, inputs}

      9 -> # Adjust relative base
        new_relbase = relbase + get_param(data, ip + 1, Enum.at(modes, 0), relbase)
        {:continue, %{machine | relbase: new_relbase, ip: ip + 2}, inputs}

      99 -> {:halt, machine}
    end
  end

  defp decode(instruction) do
    op = rem(instruction, 100)
    modes = 
      instruction 
      |> div(100) 
      |> Integer.digits() 
      |> Enum.reverse()
      |> Kernel.++([0,0,0])
      |> Enum.take(3)

    {op, modes}
  end

  defp get_param(data, pos, mode, relbase) do
    case mode do
      0 -> Map.get(data, Map.get(data, pos, 0), 0)
      1 -> Map.get(data, pos, 0)
      2 -> Map.get(data, relbase + Map.get(data, pos, 0), 0)
    end
  end

  defp set_param(data, pos, mode, relbase, val) do
    case mode do
      0 -> Map.put(data, Map.get(data, pos, 0), val)
      2 -> Map.put(data, relbase + Map.get(data, pos, 0), val)
    end
  end
end

defmodule Maze do
  def solve(program) do
    machine = IntcodeComputer.new(program)
    explore(machine, {0, 0}, %{}, MapSet.new([{0, 0}]))
  end

  defp explore(machine, pos, grid, visited) do
    Enum.reduce_while([1,2,3,4], {machine, pos, grid, visited}, fn dir, {curr_machine, curr_pos, curr_grid, curr_visited} ->
      new_pos = move(curr_pos, dir)
      
      if MapSet.member?(curr_visited, new_pos) do
        {:cont, {curr_machine, curr_pos, curr_grid, curr_visited}}
      else
        case move_robot(curr_machine, dir) do
          {:wall, _machine} -> 
            {:cont, {curr_machine, curr_pos, Map.put(curr_grid, new_pos, :wall), curr_visited}}
          {:moved, new_machine, new_pos} -> 
            {:cont, {new_machine, new_pos, Map.put(curr_grid, new_pos, :open), MapSet.put(curr_visited, new_pos)}}
          {:oxygen, new_machine, new_pos} -> 
            {:halt, {new_machine, new_pos, Map.put(curr_grid, new_pos, :oxygen), MapSet.put(curr_visited, new_pos)}}
        end
      end
    end)
    |> case do
      {:halted, {machine, pos, grid, visited}} -> 
        {machine, pos, grid, visited}
      {:cont, {machine, pos, grid, visited}} -> 
        explore(machine, pos, grid, visited)
    end
  end

  defp move({x, y}, 1), do: {x, y+1}
  defp move({x, y}, 2), do: {x, y-1}
  defp move({x, y}, 3), do: {x-1, y}
  defp move({x, y}, 4), do: {x+1, y}

  defp move_robot(machine, dir) do
    case IntcodeComputer.run(machine, [dir]) do
      [0] -> {:wall, machine}
      [1] -> {:moved, machine, dir}
      [2] -> {:oxygen, machine, dir}
    end
  end

  def shortest_path(grid, start, target) do
    bfs(grid, [{start, 0}], MapSet.new([start]), target)
  end

  defp bfs(grid, queue, visited, target) do
    case queue do
      [] -> 
        :not_found
      [{curr, dist} | rest] when curr == target -> 
        dist
      [{curr, dist} | rest] ->
        neighbors = 
          [{0,1}, {0,-1}, {1,0}, {-1,0}]
          |> Enum.map(fn {dx, dy} -> {curr.x + dx, curr.y + dy} end)
          |> Enum.filter(fn pos -> 
            Map.get(grid, pos) in [:open, :oxygen] and not MapSet.member?(visited, pos) 
          end)

        new_queue = rest ++ Enum.map(neighbors, fn pos -> {pos, dist + 1} end)
        new_visited = Enum.reduce(neighbors, visited, &MapSet.put(&2, &1))

        bfs(grid, new_queue, new_visited, target)
    end
  end
end

# Read input from file
{:ok, input} = File.read("input.txt")
program = input 
  |> String.trim() 
  |> String.split(",") 
  |> Enum.map(&String.to_integer/1)

# Solve the problem
{_machine, oxygen_pos, grid, _visited} = Maze.solve(program)
shortest_distance = Maze.shortest_path(grid, {0, 0}, oxygen_pos)

IO.puts(shortest_distance)
