
defmodule Intcode do
  defstruct memory: %{}, ip: 0, rb: 0, input: [], output: []

  def load(filename) do
    {:ok, content} = File.read(filename)
    memory = content
    |> String.trim()
    |> String.split(",")
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {val_str, index}, acc -> Map.put(acc, index, String.to_integer(val_str)) end)
    %Intcode{memory: memory}
  end

  def run(vm_state) do
    memory = vm_state.memory
    ip = vm_state.ip
    rb = vm_state.rb

    instruction = Map.get(memory, ip, 0)
    opcode = rem(instruction, 100)
    modes = [rem(div(instruction, 100), 10), rem(div(instruction, 1000), 10), rem(div(instruction, 10000), 10)]

    get_param = fn index ->
      mode = Enum.at(modes, index - 1)
      val = Map.get(memory, ip + index, 0)
      case mode do
        0 -> Map.get(memory, val, 0)
        1 -> val
        2 -> Map.get(memory, rb + val, 0)
      end
    end

    get_address = fn index ->
      mode = Enum.at(modes, index - 1)
      val = Map.get(memory, ip + index, 0)
      case mode do
        0 -> val
        2 -> rb + val
      end
    end

    case opcode do
      1 -> run(%{vm_state | memory: Map.put(memory, get_address.(3), get_param.(1) + get_param.(2)), ip: ip + 4})
      2 -> run(%{vm_state | memory: Map.put(memory, get_address.(3), get_param.(1) * get_param.(2)), ip: ip + 4})
      3 ->
        [h | t] = vm_state.input
        run(%{vm_state | memory: Map.put(memory, get_address.(1), h), input: t, ip: ip + 2})
      4 -> run(%{vm_state | output: vm_state.output ++ [get_param.(1)], ip: ip + 2})
      5 -> run(%{vm_state | ip: if(get_param.(1) != 0, do: get_param.(2), else: ip + 3)})
      6 -> run(%{vm_state | ip: if(get_param.(1) == 0, do: get_param.(2), else: ip + 3)})
      7 -> run(%{vm_state | memory: Map.put(memory, get_address.(3), if(get_param.(1) < get_param.(2), do: 1, else: 0)), ip: ip + 4})
      8 -> run(%{vm_state | memory: Map.put(memory, get_address.(3), if(get_param.(1) == get_param.(2), do: 1, else: 0)), ip: ip + 4})
      9 -> run(%{vm_state | rb: rb + get_param.(1), ip: ip + 2})
      99 -> vm_state
      _ -> raise "Unknown opcode #{opcode} at ip #{ip}"
    end
  end
end

defmodule Day19 do
  def beam(x, y, program_memory) do
    vm = %Intcode{memory: program_memory, input: [x, y]}
    %{output: [result | _]} = Intcode.run(vm)
    result == 1
  end

  def main() do
    program_memory = Intcode.load("input.txt").memory

    count =
      for y <- 0..49, x <- 0..49, beam(x, y, program_memory) do
        1
      end
      |> Enum.sum()

    IO.puts(count)
  end
end

Day19.main()
