defmodule VM do
  defstruct code: %{}, ip: 0, input: [], output: [], rb: 0

  def new(filename) do
    code = load_code(filename)
    %VM{code: code, ip: 0, input: [], output: [], rb: 0}
  end

  defp load_code(filename) do
    content = File.read!(filename)
    values = content |> String.trim() |> String.split(",") |> Enum.map(&String.to_integer/1)
    for {val, idx} <- Enum.with_index(values), into: %{}, do: {idx, val}
  end

  def run(vm) do
    do_run(vm)
  end

  defp do_run(vm) do
    code = vm.code
    ip = vm.ip
    instr = Map.get(code, ip, 0)
    op = rem(instr, 100)

    case op do
      1 ->
        [a, b, c] = param_addrs(vm, ip, instr, 3)
        va = Map.get(code, a, 0)
        vb = Map.get(code, b, 0)
        code2 = Map.put(code, c, va + vb)
        vm2 = %{vm | code: code2, ip: ip + 4}
        do_run(vm2)

      2 ->
        [a, b, c] = param_addrs(vm, ip, instr, 3)
        va = Map.get(code, a, 0)
        vb = Map.get(code, b, 0)
        code2 = Map.put(code, c, va * vb)
        vm2 = %{vm | code: code2, ip: ip + 4}
        do_run(vm2)

      3 ->
        [a] = param_addrs(vm, ip, instr, 1)
        [input_head | rest] = vm.input
        code2 = Map.put(code, a, input_head)
        vm2 = %{vm | code: code2, ip: ip + 2, input: rest}
        do_run(vm2)

      4 ->
        [a] = param_addrs(vm, ip, instr, 1)
        val = Map.get(code, a, 0)
        vm2 = %{vm | output: vm.output ++ [val], ip: ip + 2}
        do_run(vm2)

      5 ->
        [a, b] = param_addrs(vm, ip, instr, 2)
        va = Map.get(code, a, 0)
        vb = Map.get(code, b, 0)
        if va != 0 do
          vm2 = %{vm | ip: vb}
          do_run(vm2)
        else
          vm2 = %{vm | ip: ip + 3}
          do_run(vm2)
        end

      6 ->
        [a, b] = param_addrs(vm, ip, instr, 2)
        va = Map.get(code, a, 0)
        vb = Map.get(code, b, 0)
        if va == 0 do
          vm2 = %{vm | ip: vb}
          do_run(vm2)
        else
          vm2 = %{vm | ip: ip + 3}
          do_run(vm2)
        end

      7 ->
        [a, b, c] = param_addrs(vm, ip, instr, 3)
        va = Map.get(code, a, 0)
        vb = Map.get(code, b, 0)
        val = if va < vb, do: 1, else: 0
        code2 = Map.put(code, c, val)
        vm2 = %{vm | code: code2, ip: ip + 4}
        do_run(vm2)

      8 ->
        [a, b, c] = param_addrs(vm, ip, instr, 3)
        va = Map.get(code, a, 0)
        vb = Map.get(code, b, 0)
        val = if va == vb, do: 1, else: 0
        code2 = Map.put(code, c, val)
        vm2 = %{vm | code: code2, ip: ip + 4}
        do_run(vm2)

      9 ->
        [a] = param_addrs(vm, ip, instr, 1)
        va = Map.get(code, a, 0)
        vm2 = %{vm | rb: vm.rb + va, ip: ip + 2}
        do_run(vm2)

      99 ->
        vm

      _ ->
        raise "Not an opcode: #{instr}"
    end
  end

  defp param_addrs(vm, ip, instr, arity) do
    modes = get_modes(instr, arity)
    Enum.map(Enum.with_index(modes), fn {mode, idx} ->
      pos = ip + idx + 1
      param = Map.get(vm.code, pos, 0)
      case mode do
        0 -> param
        1 -> pos
        2 -> vm.rb + param
      end
    end)
  end

  defp get_modes(instr, arity) do
    mode_section = div(instr, 100)
    do_get_modes(mode_section, arity, [])
  end

  defp do_get_modes(section, 0, acc), do: Enum.reverse(acc)
  defp do_get_modes(section, arity, acc) do
    mode = rem(section, 10)
    do_get_modes(div(section, 10), arity - 1, [mode | acc])
  end
end

defmodule Solver do
  def beam(x, y) do
    vm = VM.new("input.txt")
    vm = %{vm | input: [x, y]}
    vm2 = VM.run(vm)
    List.last(vm2.output) == 1
  end
end

defmodule Main do
  def main do
    y = 20
    x = 0
    loop(x, y)
  end

  def loop(x, y) do
    cond do
      !Solver.beam(x, y) ->
        loop(x + 1, y)
      !Solver.beam(x + 99, y) ->
        loop(x, y + 1)
      !Solver.beam(x, y + 99) ->
        loop(x + 1, y)
      true ->
        IO.puts(x * 10000 + y)
    end
  end
end

Main.main()