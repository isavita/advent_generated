
defmodule VM do
  defstruct code: [], ip: 0, input: :queue.new(), halted: false

  def new(code, inputs) do
    %VM{code: code, input: :queue.from_list(inputs)}
  end

  def push_input(vm, v), do: %{vm | input: :queue.in(v, vm.input)}

  def run_until_output(%VM{halted: true}=vm), do: {vm, nil}
  def run_until_output(vm) do
    case exec(vm) do
      {:output, vm2, v} -> {vm2, v}
      {:need_input, vm2} -> {:need_input, vm2}
      {:halt, vm2} -> {vm2, nil}
      {:continue, vm2} -> run_until_output(vm2)
    end
  end

  defp exec(vm) do
    instr = Enum.at(vm.code, vm.ip)
    op = rem(instr, 100)
    mode = fn n ->
      div(instr, :math.pow(10, n + 1) |> trunc) |> rem(10) == 1
    end

    case op do
      1 ->
        a = get(vm, vm.ip + 1, mode.(1))
        b = get(vm, vm.ip + 2, mode.(2))
        dst = get(vm, vm.ip + 3, true)
        code = List.replace_at(vm.code, dst, a + b)
        {:continue, %{vm | code: code, ip: vm.ip + 4}}

      2 ->
        a = get(vm, vm.ip + 1, mode.(1))
        b = get(vm, vm.ip + 2, mode.(2))
        dst = get(vm, vm.ip + 3, true)
        code = List.replace_at(vm.code, dst, a * b)
        {:continue, %{vm | code: code, ip: vm.ip + 4}}

      3 ->
        case :queue.out(vm.input) do
          {{:value, v}, q} ->
            dst = get(vm, vm.ip + 1, true)
            code = List.replace_at(vm.code, dst, v)
            {:continue, %{vm | code: code, ip: vm.ip + 2, input: q}}

          {:empty, _} ->
            {:need_input, vm}
        end

      4 ->
        v = get(vm, vm.ip + 1, mode.(1))
        {:output, %{vm | ip: vm.ip + 2}, v}

      5 ->
        a = get(vm, vm.ip + 1, mode.(1))
        b = get(vm, vm.ip + 2, mode.(2))
        ip = if a != 0, do: b, else: vm.ip + 3
        {:continue, %{vm | ip: ip}}

      6 ->
        a = get(vm, vm.ip + 1, mode.(1))
        b = get(vm, vm.ip + 2, mode.(2))
        ip = if a == 0, do: b, else: vm.ip + 3
        {:continue, %{vm | ip: ip}}

      7 ->
        a = get(vm, vm.ip + 1, mode.(1))
        b = get(vm, vm.ip + 2, mode.(2))
        dst = get(vm, vm.ip + 3, true)
        code = List.replace_at(vm.code, dst, if(a < b, do: 1, else: 0))
        {:continue, %{vm | code: code, ip: vm.ip + 4}}

      8 ->
        a = get(vm, vm.ip + 1, mode.(1))
        b = get(vm, vm.ip + 2, mode.(2))
        dst = get(vm, vm.ip + 3, true)
        code = List.replace_at(vm.code, dst, if(a == b, do: 1, else: 0))
        {:continue, %{vm | code: code, ip: vm.ip + 4}}

      99 ->
        {:halt, %{vm | halted: true}}
    end
  end

  defp get(vm, addr, true), do: Enum.at(vm.code, addr)
  defp get(vm, addr, false), do: Enum.at(vm.code, Enum.at(vm.code, addr))
end

defmodule Main do
  @code File.read!("input.txt")
        |> String.trim()
        |> String.split(",", trim: true)
        |> Enum.map(&String.to_integer/1)

  def permute([]), do: [[]]
  def permute(list) do
    for h <- list, t <- permute(list -- [h]), do: [h | t]
  end

  def run_loop(phase) do
    vms = Enum.map(phase, &VM.new(@code, [&1]))
    vms = List.update_at(vms, 0, &VM.push_input(&1, 0))
    loop(vms, 0)
  end

  defp loop(vms, last) do
    {vms, out, halted} =
      Enum.reduce(0..4, {vms, last, false}, fn i, {vs, o, _} ->
        vm = Enum.at(vs, i)
        case VM.run_until_output(vm) do
          {:need_input, vm2} -> {List.replace_at(vs, i, vm2), o, false}
          {vm2, nil} -> {List.replace_at(vs, i, vm2), o, vm2.halted}
          {vm2, v} ->
            nxt = rem(i + 1, 5)
            vs = List.update_at(vs, nxt, &VM.push_input(&1, v))
            {List.replace_at(vs, i, vm2), v, vm2.halted}
        end
      end)

    if Enum.all?(vms, & &1.halted), do: out, else: loop(vms, out)
  end

  def main do
    max =
      permute([5, 6, 7, 8, 9])
      |> Enum.map(&run_loop/1)
      |> Enum.max()

    IO.puts(max)
  end
end

Main.main()
