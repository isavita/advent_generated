
defmodule Solution do
  def main do
    program = File.read!("input.txt")
    |> String.trim()
    |> String.split(",")
    |> Enum.with_index()
    |> Enum.into(%{}, fn {v, i} -> {i, String.to_integer(v)} end)

    computers = Enum.map(0..49, fn i ->
      %{mem: program, ip: 0, base: 0, inputs: [i], outputs: [], idle: false}
    end)

    loop(computers, Map.new(0..49, fn i -> {i, []} end), nil, nil)
  end

  def loop(computers, queues, nat, last_y) do
    {new_computers, {new_queues, new_nat}} = Enum.map_reduce(Enum.with_index(computers), {queues, nat}, fn {c, i}, {q_acc, n_acc} ->
      {c_in, q_rem} = case Map.get(q_acc, i) do
        [] -> {c, []}
        [{x, y} | rest] -> {%{c | inputs: c.inputs ++ [x, y], idle: false}, rest}
      end
      {c_res, packets} = run_computer(c_in, [])
      {next_q, next_n} = Enum.reduce(packets, {Map.put(q_acc, i, q_rem), n_acc}, fn {d, x, y}, {qa, na} ->
        if d == 255, do: {qa, {x, y}}, else: {Map.update!(qa, d, & &1 ++ [{x, y}]), na}
      end)
      {c_res, {next_q, next_n}}
    end)

    if Enum.all?(new_computers, & &1.idle) and Enum.all?(0..49, &(Map.get(new_queues, &1) == [])) do
      if new_nat do
        {nx, ny} = new_nat
        if ny == last_y do
          IO.puts(ny)
        else
          new_computers = List.update_at(new_computers, 0, fn c0 -> %{c0 | inputs: c0.inputs ++ [nx, ny], idle: false} end)
          loop(new_computers, new_queues, new_nat, ny)
        end
      else
        loop(new_computers, new_queues, new_nat, last_y)
      end
    else
      loop(new_computers, new_queues, new_nat, last_y)
    end
  end

  def run_computer(c, pkts) do
    op_f = Map.get(c.mem, c.ip, 0)
    {op, modes} = {rem(op_f, 100), div(op_f, 100)}
    m1 = rem(modes, 10); m2 = rem(div(modes, 10), 10); m3 = rem(div(modes, 100), 10)
    case op do
      99 -> {%{c | idle: true}, pkts}
      1 -> run_computer(%{set_v(c, 3, m3, get_v(c, 1, m1) + get_v(c, 2, m2)) | ip: c.ip + 4, idle: false}, pkts)
      2 -> run_computer(%{set_v(c, 3, m3, get_v(c, 1, m1) * get_v(c, 2, m2)) | ip: c.ip + 4, idle: false}, pkts)
      3 ->
        case c.inputs do
          [] -> {%{set_v(c, 1, m1, -1) | ip: c.ip + 2, idle: true}, pkts}
          [h | t] -> run_computer(%{set_v(c, 1, m1, h) | ip: c.ip + 2, inputs: t, idle: false}, pkts)
        end
      4 ->
        v = get_v(c, 1, m1)
        case c.outputs do
          [dx, dy] -> {%{c | outputs: [], ip: c.ip + 2, idle: false}, pkts ++ [{dx, dy, v}]}
          _ -> run_computer(%{c | outputs: c.outputs ++ [v], ip: c.ip + 2, idle: false}, pkts)
        end
      5 -> run_computer(%{c | ip: if(get_v(c, 1, m1) != 0, do: get_v(c, 2, m2), else: c.ip + 3), idle: false}, pkts)
      6 -> run_computer(%{c | ip: if(get_v(c, 1, m1) == 0, do: get_v(c, 2, m2), else: c.ip + 3), idle: false}, pkts)
      7 -> run_computer(%{set_v(c, 3, m3, if(get_v(c, 1, m1) < get_v(c, 2, m2), do: 1, else: 0)) | ip: c.ip + 4, idle: false}, pkts)
      8 -> run_computer(%{set_v(c, 3, m3, if(get_v(c, 1, m1) == get_v(c, 2, m2), do: 1, else: 0)) | ip: c.ip + 4, idle: false}, pkts)
      9 -> run_computer(%{c | base: c.base + get_v(c, 1, m1), ip: c.ip + 2, idle: false}, pkts)
    end
  end

  def get_v(c, off, mode) do
    pos = Map.get(c.mem, c.ip + off, 0)
    case mode do
      0 -> Map.get(c.mem, pos, 0)
      1 -> pos
      2 -> Map.get(c.mem, c.base + pos, 0)
    end
  end

  def set_v(c, off, mode, val) do
    pos = Map.get(c.mem, c.ip + off, 0)
    addr = if mode == 2, do: c.base + pos, else: pos
    %{c | mem: Map.put(c.mem, addr, val)}
  end
end

Solution.main()
