defmodule Solver do
  defmodule Pos do
    defstruct [:i, :j]
  end

  def find_position(mat, ch) do
    case Enum.find_index(mat, fn row -> Enum.member?(row, ch) end) do
      nil ->
        %Pos{i: -1, j: -1}
      i ->
        row = Enum.at(mat, i)
        j = Enum.find_index(row, fn x -> x == ch end)
        %Pos{i: i, j: j}
    end
  end

  def get_char(mat, i, j) do
    Enum.at(Enum.at(mat, i), j)
  end

  def ok(mat, %Pos{i: i, j: j}, seq) do
    ok_loop(mat, %Pos{i: i, j: j}, seq)
  end

  defp ok_loop(_mat, %Pos{i: i, j: j}, []) do
    true
  end

  defp ok_loop(mat, %Pos{i: i, j: j}, [c | rest]) do
    height = length(mat)
    width = length(Enum.at(mat, 0))
    if get_char(mat, i, j) == 32 do
      false
    else
      curr = case c do
        94 -> %Pos{i: i - 1, j: j}
        118 -> %Pos{i: i + 1, j: j}
        60 -> %Pos{i: i, j: j - 1}
        62 -> %Pos{i: i, j: j + 1}
        _ -> %Pos{i: i, j: j}
      end
      if curr.i < 0 or curr.i >= height or curr.j < 0 or curr.j >= width do
        false
      else
        ok_loop(mat, curr, rest)
      end
    end
  end

  def generate_moves(pos, objective, pad) do
    obj_pos = find_position(pad, objective)
    ret1 = ""
    ret1 = if pos.j > obj_pos.j, do: ret1 <> String.duplicate("<", pos.j - obj_pos.j), else: ret1
    ret1 = if pos.i > obj_pos.i, do: ret1 <> String.duplicate("^", pos.i - obj_pos.i), else: ret1
    ret1 = if pos.i < obj_pos.i, do: ret1 <> String.duplicate("v", obj_pos.i - pos.i), else: ret1
    ret1 = if pos.j < obj_pos.j, do: ret1 <> String.duplicate(">", obj_pos.j - pos.j), else: ret1

    if ok(pad, pos, String.to_charlist(ret1)) do
      ret1
    else
      ret2 = ""
      ret2 = if pos.j < obj_pos.j, do: ret2 <> String.duplicate(">", obj_pos.j - pos.j), else: ret2
      ret2 = if pos.i > obj_pos.i, do: ret2 <> String.duplicate("^", pos.i - obj_pos.i), else: ret2
      ret2 = if pos.i < obj_pos.i, do: ret2 <> String.duplicate("v", obj_pos.i - pos.i), else: ret2
      ret2 = if pos.j > obj_pos.j, do: ret2 <> String.duplicate("<", pos.j - obj_pos.j), else: ret2
      ret2
    end
  end

  def solve(code, robots, keyPad, robotPad, maxRobots) when robots <= 0 do
    byte_size(code)
  end

  def solve(code, robots, keyPad, robotPad, maxRobots) do
    posi = if robots == maxRobots, do: 3, else: 0
    posj = 2
    chars = String.to_charlist(code)
    {ret, _pi, _pj} =
      Enum.reduce(chars, {0, posi, posj}, fn ch, {acc, pi, pj} ->
        if robots == maxRobots do
          moves = generate_moves(%Pos{i: pi, j: pj}, ch, keyPad)
          pos = find_position(keyPad, ch)
          acc2 = acc + solve(moves <> "A", robots - 1, keyPad, robotPad, maxRobots)
          {acc2, pos.i, pos.j}
        else
          moves = generate_moves(%Pos{i: pi, j: pj}, ch, robotPad)
          pos = find_position(robotPad, ch)
          acc2 = acc + solve(moves <> "A", robots - 1, keyPad, robotPad, maxRobots)
          {acc2, pos.i, pos.j}
        end
      end)
    ret
  end

  def main do
    maxRobots = 3
    keyPad = [
      '789',
      '456',
      '123',
      ' 0A'
    ]
    robotPad = [
      ' ^A',
      '<v>'
    ]

    case File.read("input.txt") do
      {:ok, content} ->
        codes = content |> String.trim() |> String.split("\n", trim: true)
        total = Enum.reduce(codes, 0, fn code, acc ->
          code = String.trim(code)
          if code == "" do
            acc
          else
            digits = code |> String.to_charlist()
            numeric =
              digits
              |> Enum.filter(&(&1 >= 48 and &1 <= 57))
              |> Enum.reduce(0, fn d, a -> a * 10 + (d - 48) end)
            sv = solve(code, maxRobots, keyPad, robotPad, maxRobots)
            acc + sv * numeric
          end
        end)
        IO.puts(total)
      {:error, reason} ->
        IO.puts("Error reading file: #{reason}")
    end
  end
end

Solver.main()