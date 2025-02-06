
defmodule Intcode do
  def decode(n) do
    op = rem(n, 100)
    modes = div(n, 100)
    |> to_string()
    |> String.pad_leading(3, "0")
    |> String.graphemes()
    |> Enum.reverse()
    |> Enum.map(&String.to_integer/1)
    |> Enum.map(fn
      0 -> :position
      1 -> :immediate
      2 -> :relative
    end)
    {opcode(op), modes}
  end

  def opcode(1), do: :add
  def opcode(2), do: :mul
  def opcode(3), do: :input
  def opcode(4), do: :output
  def opcode(5), do: :jt
  def opcode(6), do: :jf
  def opcode(7), do: :lt
  def opcode(8), do: :eq
  def opcode(9), do: :rbo
  def opcode(99), do: :halt
  def opcode(op), do: raise("Unknown opcode: #{op}")

  def new(program, input, output) do
    data =
      program
      |> Enum.with_index()
      |> Enum.into(%{}, fn {val, idx} -> {idx, val} end)

    %{
      data: data,
      ip: 0,
      input: input,
      output: output,
      relbase: 0
    }
  end

  def get_value(machine, addr, mode) do
    data = machine.data
    relbase = machine.relbase

    case mode do
      :immediate ->
        Map.get(data, addr, 0)

      :position ->
        pos = Map.get(data, addr, 0)
        Map.get(data, pos, 0)

      :relative ->
        pos = Map.get(data, addr, 0)
        Map.get(data, relbase + pos, 0)

      _ ->
        raise("Unknown mode: #{mode}")
    end
  end

  def set_value(machine, addr, mode, val) do
    data = machine.data
    relbase = machine.relbase

    case mode do
      :position ->
        pos = Map.get(data, addr, 0)
        Map.put(data, pos, val)

      :relative ->
        pos = Map.get(data, addr, 0)
        Map.put(data, relbase + pos, val)

      _ ->
        raise("Unknown mode: #{mode}")
    end
  end

  def step(machine) do
    ip = machine.ip
    data = machine.data
    relbase = machine.relbase

    {op, modes} = decode(Map.get(data, ip, 0))

    case op do
      :add ->
        val = get_value(machine, ip + 1, Enum.at(modes, 0)) + get_value(machine, ip + 2, Enum.at(modes, 1))
        data = set_value(machine, ip + 3, Enum.at(modes, 2), val)
        %{machine | data: data, ip: ip + 4}

      :mul ->
        val = get_value(machine, ip + 1, Enum.at(modes, 0)) * get_value(machine, ip + 2, Enum.at(modes, 1))
        data = set_value(machine, ip + 3, Enum.at(modes, 2), val)
        %{machine | data: data, ip: ip + 4}

      :input ->
        input_val = Enum.at(machine.input, 0)
        data = set_value(machine, ip + 1, Enum.at(modes, 0), input_val)
        %{machine | data: data, ip: ip + 2, input: tl(machine.input)}

      :output ->
        output_val = get_value(machine, ip + 1, Enum.at(modes, 0))
        %{machine | output: machine.output ++ [output_val], ip: ip + 2}

      :jt ->
        if get_value(machine, ip + 1, Enum.at(modes, 0)) != 0 do
          %{machine | ip: get_value(machine, ip + 2, Enum.at(modes, 1))}
        else
          %{machine | ip: ip + 3}
        end

      :jf ->
        if get_value(machine, ip + 1, Enum.at(modes, 0)) == 0 do
          %{machine | ip: get_value(machine, ip + 2, Enum.at(modes, 1))}
        else
          %{machine | ip: ip + 3}
        end

      :lt ->
        val = if get_value(machine, ip + 1, Enum.at(modes, 0)) < get_value(machine, ip + 2, Enum.at(modes, 1)), do: 1, else: 0
        data = set_value(machine, ip + 3, Enum.at(modes, 2), val)
        %{machine | data: data, ip: ip + 4}

      :eq ->
        val = if get_value(machine, ip + 1, Enum.at(modes, 0)) == get_value(machine, ip + 2, Enum.at(modes, 1)), do: 1, else: 0
        data = set_value(machine, ip + 3, Enum.at(modes, 2), val)
        %{machine | data: data, ip: ip + 4}

      :rbo ->
        relbase = relbase + get_value(machine, ip + 1, Enum.at(modes, 0))
        %{machine | relbase: relbase, ip: ip + 2}

      :halt ->
        {:halted, machine}

      _ ->
        raise("Unknown opcode: #{op}")
    end
  end

  def run(machine) do
    case step(machine) do
      {:halted, machine} ->
        machine.output

      next_machine ->
        run(next_machine)
    end
  end

  def start_machine(program, input \\ []) do
    machine = new(program, input, [])
    run(machine)
  end
end

defmodule Day13 do
  def count_blocks(program) do
    output = Intcode.start_machine(program)
    grid = build_grid(output)
    count_block_tiles(grid)
  end

  def build_grid(output) do
    output
    |> Enum.chunk_every(3)
    |> Enum.reduce(%{}, fn [x, y, tile_id], acc ->
      Map.put(acc, {x, y}, tile_id)
    end)
  end

  def count_block_tiles(grid) do
    grid
    |> Map.values()
    |> Enum.count(fn tile_id -> tile_id == 2 end)
  end

  def read_program_from_file(file_path) do
    File.read!(file_path)
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  def solve() do
    program = read_program_from_file("input.txt")
    answer = count_blocks(program)
    IO.puts(answer)
  end
end

Day13.solve()
