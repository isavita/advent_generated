
defmodule Solution do
  defstruct memory: %{}, ip: 0, input: [], output: [], relative_base: 0

  def load(filename) do
    {:ok, content} = File.read(filename)
    program =
      content
      |> String.trim()
      |> String.split(",")
      |> Enum.map(&String.to_integer/1)
      |> Enum.with_index()
      |> Map.new(fn {val, idx} -> {idx, val} end)

    %__MODULE__{memory: program}
  end

  defp get_param(%__MODULE__{memory: memory, ip: ip, relative_base: rb} = vm, index, mode) do
    address_or_value = Map.get(memory, ip + index, 0)
    case mode do
      0 -> Map.get(memory, address_or_value, 0)
      1 -> address_or_value
      2 -> Map.get(memory, rb + address_or_value, 0)
    end
  end

  defp get_address(%__MODULE__{memory: memory, ip: ip, relative_base: rb}, index, mode) do
    address_or_value = Map.get(memory, ip + index, 0)
    case mode do
      0 -> address_or_value
      2 -> rb + address_or_value
      _ -> raise "Invalid mode #{mode} for write address"
    end
  end

  def run(%__MODULE__{} = vm) do
    instruction = Map.get(vm.memory, vm.ip, 0)
    opcode = rem(instruction, 100)
    modes = [(div(instruction, 100) |> rem(10)), (div(instruction, 1000) |> rem(10)), (div(instruction, 10000) |> rem(10))]

    case opcode do
      1 ->
        p1 = get_param(vm, 1, Enum.at(modes, 0))
        p2 = get_param(vm, 2, Enum.at(modes, 1))
        address = get_address(vm, 3, Enum.at(modes, 2))
        new_memory = Map.put(vm.memory, address, p1 + p2)
        run(%{vm | memory: new_memory, ip: vm.ip + 4})

      2 ->
        p1 = get_param(vm, 1, Enum.at(modes, 0))
        p2 = get_param(vm, 2, Enum.at(modes, 1))
        address = get_address(vm, 3, Enum.at(modes, 2))
        new_memory = Map.put(vm.memory, address, p1 * p2)
        run(%{vm | memory: new_memory, ip: vm.ip + 4})

      3 ->
        case vm.input do
          [] -> raise "Input buffer empty!"
          [h | t] ->
            address = get_address(vm, 1, Enum.at(modes, 0))
            new_memory = Map.put(vm.memory, address, h)
            run(%{vm | memory: new_memory, ip: vm.ip + 2, input: t})
        end

      4 ->
        p1 = get_param(vm, 1, Enum.at(modes, 0))
        run(%{vm | output: vm.output ++ [p1], ip: vm.ip + 2})

      5 ->
        p1 = get_param(vm, 1, Enum.at(modes, 0))
        p2 = get_param(vm, 2, Enum.at(modes, 1))
        new_ip = if p1 != 0, do: p2, else: vm.ip + 3
        run(%{vm | ip: new_ip})

      6 ->
        p1 = get_param(vm, 1, Enum.at(modes, 0))
        p2 = get_param(vm, 2, Enum.at(modes, 1))
        new_ip = if p1 == 0, do: p2, else: vm.ip + 3
        run(%{vm | ip: new_ip})

      7 ->
        p1 = get_param(vm, 1, Enum.at(modes, 0))
        p2 = get_param(vm, 2, Enum.at(modes, 1))
        address = get_address(vm, 3, Enum.at(modes, 2))
        value = if p1 < p2, do: 1, else: 0
        new_memory = Map.put(vm.memory, address, value)
        run(%{vm | memory: new_memory, ip: vm.ip + 4})

      8 ->
        p1 = get_param(vm, 1, Enum.at(modes, 0))
        p2 = get_param(vm, 2, Enum.at(modes, 1))
        address = get_address(vm, 3, Enum.at(modes, 2))
        value = if p1 == p2, do: 1, else: 0
        new_memory = Map.put(vm.memory, address, value)
        run(%{vm | memory: new_memory, ip: vm.ip + 4})

      9 ->
        p1 = get_param(vm, 1, Enum.at(modes, 0))
        run(%{vm | relative_base: vm.relative_base + p1, ip: vm.ip + 2})

      99 -> vm

      _ -> raise "Unknown opcode #{opcode} at ip #{vm.ip}"
    end
  end

  def send_string(%__MODULE__{} = vm, s) do
    chars = String.to_charlist(s) ++ [?\n]
    %{vm | input: vm.input ++ chars}
  end

  def main do
    vm = load("input.txt")

    instructions = [
        "NOT A J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "WALK",
    ]

    vm_with_input = Enum.reduce(instructions, vm, fn instr, acc_vm ->
      send_string(acc_vm, instr)
    end)

    final_vm = run(vm_with_input)

    final_vm.output
    |> Enum.find(&(&1 > 127))
    |> IO.puts()
  end
end

Solution.main()
