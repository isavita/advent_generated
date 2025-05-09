
defmodule IntcodeAoc do
  defstruct memory: %{}, ip: 0, relbase: 0, input: [], output: []

  def main do
    program_file = "input.txt"
    case read_program(program_file) do
      {:ok, program_map} ->
        final_state = run(program_map)
        output = final_state.output

        grid = parse_output(output)
        alignment_sum = sum_alignment(grid)

        IO.puts(alignment_sum)
      {:error, reason} ->
        IO.puts("Error reading file '#{program_file}': #{reason}")
    end
  end

  defp read_program(filename) do
    case File.read(filename) do
      {:ok, content} ->
        program =
          content
          |> String.trim()
          |> String.split(",")
          |> Enum.map(&String.to_integer/1)
        program_map =
          Enum.with_index(program)
          |> Map.new(fn {val, index} -> {index, val} end)
        {:ok, program_map}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp initial_state(program_map, input_list \\ []) do
    %__MODULE__{
      memory: program_map,
      ip: 0,
      relbase: 0,
      input: input_list,
      output: []
    }
  end

  defp decode(instruction) do
    opcode = rem(instruction, 100)
    mode1 = rem(div(instruction, 100), 10)
    mode2 = rem(div(instruction, 1000), 10)
    mode3 = rem(div(instruction, 10000), 10)
    {opcode, [mode1, mode2, mode3]}
  end

  defp get(memory, addr, mode, relbase) do
    case mode do
      0 -> Map.get(memory, Map.get(memory, addr, 0), 0)
      1 -> Map.get(memory, addr, 0)
      2 -> Map.get(memory, relbase + Map.get(memory, addr, 0), 0)
      _ -> raise "Invalid parameter mode: #{mode}"
    end
  end

  defp set(memory, addr, mode, relbase, value) do
    write_addr =
      case mode do
        0 -> Map.get(memory, addr, 0)
        2 -> relbase + Map.get(memory, addr, 0)
        _ -> raise "Invalid write mode: #{mode} at address #{addr}"
      end
    Map.put(memory, write_addr, value)
  end

  defp step(%__MODULE__{} = state) do
    %{memory: memory, ip: ip, relbase: relbase, input: input, output: output} = state

    instruction = Map.get(memory, ip, 0)
    {opcode, modes} = decode(instruction)

    get_param = fn offset, mode -> get(memory, ip + offset, mode, relbase) end
    set_param = fn offset, mode, value -> set(memory, ip + offset, mode, relbase, value) end

    case opcode do
      1 ->
        new_memory = set_param.(3, Enum.at(modes, 2), get_param.(1, Enum.at(modes, 0)) + get_param.(2, Enum.at(modes, 1)))
        {%{state | memory: new_memory, ip: ip + 4}, :continue}
      2 ->
        new_memory = set_param.(3, Enum.at(modes, 2), get_param.(1, Enum.at(modes, 0)) * get_param.(2, Enum.at(modes, 1)))
        {%{state | memory: new_memory, ip: ip + 4}, :continue}
      3 ->
        case input do
          [h | t] ->
            new_memory = set_param.(1, Enum.at(modes, 0), h)
            {%{state | memory: new_memory, ip: ip + 2, input: t}, :continue}
          [] ->
            raise "Input requested but none available at ip #{ip}"
        end
      4 ->
        val = get_param.(1, Enum.at(modes, 0))
        {%{state | output: output ++ [val], ip: ip + 2}, :continue}
      5 ->
        new_ip = if get_param.(1, Enum.at(modes, 0)) != 0, do: get_param.(2, Enum.at(modes, 1)), else: ip + 3
        {%{state | ip: new_ip}, :continue}
      6 ->
        new_ip = if get_param.(1, Enum.at(modes, 0)) == 0, do: get_param.(2, Enum.at(modes, 1)), else: ip + 3
        {%{state | ip: new_ip}, :continue}
      7 ->
        new_memory = set_param.(3, Enum.at(modes, 2), if(get_param.(1, Enum.at(modes, 0)) < get_param.(2, Enum.at(modes, 1)), do: 1, else: 0))
        {%{state | memory: new_memory, ip: ip + 4}, :continue}
      8 ->
        new_memory = set_param.(3, Enum.at(modes, 2), if(get_param.(1, Enum.at(modes, 0)) == get_param.(2, Enum.at(modes, 1)), do: 1, else: 0))
        {%{state | memory: new_memory, ip: ip + 4}, :continue}
      9 ->
        {%{state | relbase: relbase + get_param.(1, Enum.at(modes, 0)), ip: ip + 2}, :continue}
      99 ->
        {state, :halt}
      _ ->
        raise "Unknown opcode: #{opcode} at ip #{ip}"
    end
  end

  defp run(program_map, input_list \\ []) do
    initial_state = initial_state(program_map, input_list)
    do_run_loop(initial_state)
  end

  defp do_run_loop(state) do
    case step(state) do
      {next_state, :continue} -> do_run_loop(next_state)
      {final_state, :halt} -> final_state
    end
  end

  defp chr(code), do: <<code::utf8>>

  defp parse_output(output_list) do
    output_list
    |> Enum.map(&chr/1)
    |> Enum.join("")
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {line, y}, acc ->
         line
         |> String.codepoints()
         |> Enum.with_index()
         |> Enum.reduce(acc, fn {char, x}, acc2 ->
              if char in ["#", "^", "v", "<", ">"], do: Map.put(acc2, {x, y}, char), else: acc2
         end)
    end)
  end

  defp sum_alignment(grid) do
    grid
    |> Map.keys()
    |> Enum.filter(fn {x, y} ->
         neighbors = [{x, y + 1}, {x, y - 1}, {x + 1, y}, {x - 1, y}]
         Enum.all?(neighbors, &Map.has_key?(grid, &1))
    end)
    |> Enum.reduce(0, fn {x, y}, sum ->
         sum + x * y
    end)
  end
end

IntcodeAoc.main()
