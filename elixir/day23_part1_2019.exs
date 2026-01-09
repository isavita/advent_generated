
defmodule Intcode do
  defstruct mem: %{}, ip: 0, rb: 0, in: :queue.new(), out: [], halt: false, need: false

  def new(prog, inp \\ []) do
    mem = Enum.with_index(prog) |> Enum.reduce(%{}, fn {v, i}, acc -> Map.put(acc, i, v) end)
    %Intcode{mem: mem, in: :queue.from_list(inp)}
  end

  def push(%Intcode{in: q} = c, v), do: %{c | in: :queue.in(v, q)}

  defp get(c, off) do
    m = Enum.at([c.mem[c.ip] |> div(100) |> rem(10), c.mem[c.ip] |> div(1000) |> rem(10), c.mem[c.ip] |> div(10000) |> rem(10)], off - 1)
    v = Map.get(c.mem, c.ip + off, 0)
    case m do
      0 -> Map.get(c.mem, v, 0)
      1 -> v
      2 -> Map.get(c.mem, c.rb + v, 0)
    end
  end

  defp set(c, off, val) do
    m = Enum.at([c.mem[c.ip] |> div(100) |> rem(10), c.mem[c.ip] |> div(1000) |> rem(10), c.mem[c.ip] |> div(10000) |> rem(10)], off - 1)
    v = Map.get(c.mem, c.ip + off, 0)
    addr = if m == 2, do: c.rb + v, else: v
    %{c | mem: Map.put(c.mem, addr, val)}
  end

  def step(c) do
    op = rem(c.mem[c.ip], 100)
    case op do
      99 -> %{c | halt: true}
      1 -> step(set(c, 3, get(c, 1) + get(c, 2)) |> Map.put(:ip, c.ip + 4))
      2 -> step(set(c, 3, get(c, 1) * get(c, 2)) |> Map.put(:ip, c.ip + 4))
      3 ->
        case :queue.out(c.in) do
          {:empty, _} -> %{c | need: true}
          {{:value, v}, q} -> step(set(c, 1, v) |> Map.put(:in, q) |> Map.put(:ip, c.ip + 2))
        end
      4 ->
        out = get(c, 1)
        c = %{c | out: c.out ++ [out], ip: c.ip + 2}
        if length(c.out) == 3, do: c, else: step(c)
      5 -> step(if get(c, 1) != 0, do: Map.put(c, :ip, get(c, 2)), else: Map.put(c, :ip, c.ip + 3))
      6 -> step(if get(c, 1) == 0, do: Map.put(c, :ip, get(c, 2)), else: Map.put(c, :ip, c.ip + 3))
      7 -> step(set(c, 3, if(get(c, 1) < get(c, 2), do: 1, else: 0)) |> Map.put(:ip, c.ip + 4))
      8 -> step(set(c, 3, if(get(c, 1) == get(c, 2), do: 1, else: 0)) |> Map.put(:ip, c.ip + 4))
      9 -> step(Map.put(c, :rb, c.rb + get(c, 1)) |> Map.put(:ip, c.ip + 2))
    end
  end
end

defmodule Main do
  def run do
    prog = File.read!("input.txt") |> String.trim() |> String.split(",") |> Enum.map(&String.to_integer/1)
    comps = for i <- 0..49, do: Intcode.new(prog, [i])
    queues = for _ <- 0..49, do: :queue.new()
    {comps, queues} |> loop(nil)
  end

  defp loop({comps, queues}, first) do
    {comps, queues, idle} =
      Enum.reduce(0..49, {comps, queues, true}, fn i, {cs, qs, idle} ->
        c = Enum.at(cs, i)
        q = Enum.at(qs, i)
        {c, q} =
          case :queue.out(q) do
            {:empty, _} -> {Intcode.push(c, -1), q}
            {{:value, {x, y}}, q2} -> {Intcode.push(Intcode.push(c, x), y), q2}
          end
        c = Intcode.step(c)
        case length(c.out) >= 3 do
          false -> {List.replace_at(cs, i, c), List.replace_at(qs, i, q), idle}
          true ->
            [dest, x, y] = Enum.take(c.out, 3)
            c = %{c | out: Enum.drop(c.out, 3)}
            dest = if is_integer(dest), do: dest, else: String.to_integer("#{dest}")
            x = if is_integer(x), do: x, else: String.to_integer("#{x}")
            y = if is_integer(y), do: y, else: String.to_integer("#{y}")
            if dest == 255 do
              IO.puts(y)
              System.halt(0)
            end
            if dest >= 0 and dest < 50 do
              q2 = Enum.at(qs, dest)
              qs = List.replace_at(qs, dest, :queue.in({x, y}, q2))
              {List.replace_at(cs, i, c), qs, false}
            else
              {List.replace_at(cs, i, c), List.replace_at(qs, i, q), false}
            end
        end
      end)
    loop({comps, queues}, first)
  end
end

Main.run()
