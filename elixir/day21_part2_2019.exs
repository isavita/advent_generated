defmodule IntcodeVM do
  defstruct code: %{}, ip: 0, input: [], output: [], relative_base: 0

  def new(filename) do
    code = load_program(filename)
    %IntcodeVM{code: code, ip: 0, input: [], output: [], relative_base: 0}
  end

  def load_program(filename) do
    with {:ok, data} <- File.read(filename) do
      data
      |> String.trim()
      |> String.split(",", trim: true)
      |> Enum.map(&String.to_integer/1)
      |> Enum.with_index()
      |> Enum.into(%{}, fn {val, idx} -> {idx, val} end)
    else
      _ -> %{}
    end
  end

  def push_input(vm, val), do: %{vm | input: vm.input ++ [val]}

  def pop_input(vm) do
    case vm.input do
      [h | t] -> {h, %{vm | input: t}}
      [] -> raise "input empty"
    end
  end

  def write(vm, addr, val), do: %{vm | code: Map.put(vm.code, addr, val)}
  def read(vm, addr), do: Map.get(vm.code, addr, 0)

  def get_param_value(vm, param_idx, mode) do
    addr = vm.ip + param_idx
    value_at_ip_plus = read(vm, addr)

    case mode do
      0 -> read(vm, value_at_ip_plus)
      1 -> value_at_ip_plus
      2 -> read(vm, vm.relative_base + value_at_ip_plus)
    end
  end

  def get_write_address(vm, param_idx, mode) do
    value_at_ip_plus = read(vm, vm.ip + param_idx)

    case mode do
      0 -> value_at_ip_plus
      2 -> vm.relative_base + value_at_ip_plus
      _ -> value_at_ip_plus
    end
  end

  def run(vm) do
    case step(vm) do
      {:halt, vm2} -> vm2
      {:continue, vm2} -> run(vm2)
    end
  end

  def step(vm) do
    instr = read(vm, vm.ip)
    opcode = rem(instr, 100)
    mode1 = rem(div(instr, 100), 10)
    mode2 = rem(div(instr, 1000), 10)
    mode3 = rem(div(instr, 10000), 10)

    case opcode do
      99 -> {:halt, vm}
      1 ->
        a = get_param_value(vm, 1, mode1)
        b = get_param_value(vm, 2, mode2)
        addr = get_write_address(vm, 3, mode3)
        vm2 = write(vm, addr, a + b)
        vm3 = %{vm2 | ip: vm2.ip + 4}
        {:continue, vm3}

      2 ->
        a = get_param_value(vm, 1, mode1)
        b = get_param_value(vm, 2, mode2)
        addr = get_write_address(vm, 3, mode3)
        vm2 = write(vm, addr, a * b)
        vm3 = %{vm2 | ip: vm2.ip + 4}
        {:continue, vm3}

      3 ->
        addr = get_write_address(vm, 1, mode1)
        {input_val, vm2} = pop_input(vm)
        vm3 = write(vm2, addr, input_val)
        vm4 = %{vm3 | ip: vm3.ip + 2}
        {:continue, vm4}

      4 ->
        val = get_param_value(vm, 1, mode1)
        vm2 = %{vm | ip: vm.ip + 2, output: vm.output ++ [val]}
        {:continue, vm2}

      5 ->
        cond = get_param_value(vm, 1, mode1)
        if cond != 0 do
          vm2 = %{vm | ip: get_param_value(vm, 2, mode2)}
          {:continue, vm2}
        else
          vm3 = %{vm | ip: vm.ip + 3}
          {:continue, vm3}
        end

      6 ->
        cond = get_param_value(vm, 1, mode1)
        if cond == 0 do
          vm2 = %{vm | ip: get_param_value(vm, 2, mode2)}
          {:continue, vm2}
        else
          vm3 = %{vm | ip: vm.ip + 3}
          {:continue, vm3}
        end

      7 ->
        a = get_param_value(vm, 1, mode1)
        b = get_param_value(vm, 2, mode2)
        addr = get_write_address(vm, 3, mode3)
        val = if a < b, do: 1, else: 0
        vm2 = write(vm, addr, val)
        vm3 = %{vm2 | ip: vm2.ip + 4}
        {:continue, vm3}

      8 ->
        a = get_param_value(vm, 1, mode1)
        b = get_param_value(vm, 2, mode2)
        addr = get_write_address(vm, 3, mode3)
        val = if a == b, do: 1, else: 0
        vm2 = write(vm, addr, val)
        vm3 = %{vm2 | ip: vm2.ip + 4}
        {:continue, vm3}

      9 ->
        rb = get_param_value(vm, 1, mode1)
        vm2 = %{vm | relative_base: vm.relative_base + rb, ip: vm.ip + 2}
        {:continue, vm2}

      _ ->
        {:halt, vm}
    end
  end

  def send_string(vm, s) do
    bytes = :binary.bin_to_list(s)
    vm1 = Enum.reduce(bytes, vm, fn b, acc -> push_input(acc, b) end)
    push_input(vm1, 10)
  end
end

defmodule Main do
  def main do
    vm = IntcodeVM.new("input.txt")
    instructions = [
      "NOT A J",
      "NOT B T",
      "OR T J",
      "NOT C T",
      "OR T J",
      "AND D J",
      "NOT A T",
      "AND A T",
      "OR E T",
      "OR H T",
      "AND T J",
      "RUN"
    ]

    vm2 = Enum.reduce(instructions, vm, fn line, acc -> IntcodeVM.send_string(acc, line) end)
    vm3 = IntcodeVM.run(vm2)

    Enum.each(vm3.output, fn v ->
      if v >= 0 and v <= 127 do
        IO.write(<<v>>)
      else
        IO.puts(v)
      end
    end)
  end
end

Main.main()