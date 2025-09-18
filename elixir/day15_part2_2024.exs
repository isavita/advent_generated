defmodule Solver do
  def main(_args) do
    input = File.read!("input.txt")
    ans1 = solve(input)
    ans2 = solve(scale_up(input))
    IO.puts(Integer.to_string(ans1))
    IO.puts(Integer.to_string(ans2))
  end

  def solve(input) do
    blocks = input |> String.trim() |> String.split("\n\n", parts: 2)
    grid_block = Enum.at(blocks, 0, "")
    instr_block = Enum.at(blocks, 1, "")

    lines = grid_block |> String.split("\n", trim: false)
    m0 = build_map(lines)

    instr = instr_block |> String.replace("\n", "")
    dirs = instr |> String.graphemes() |> Enum.map(&char_to_dir/1)

    robot = find_robot(m0)

    {m_final, _pos} =
      Enum.reduce(dirs, {m0, robot}, fn dir, {m, pos} ->
        case try_to_step(m, pos, dir) do
          {:ok, m1} -> {m1, add(pos, dir)}
          {:error, _} -> {m, pos}
        end
      end)

    Enum.reduce(Map.to_list(m_final), 0, fn {{x, y}, ch}, acc ->
      if ch in ["[", "O"] do
        acc + x + 100 * y
      else
        acc
      end
    end)
  end

  def scale_up(s) do
    s
    |> String.replace("#", "##")
    |> String.replace(".", "..")
    |> String.replace("O", "[]")
    |> String.replace("@", "@.")
  end

  def build_map(lines) do
    Enum.reduce(Enum.with_index(lines), %{}, fn {row, y}, acc ->
      chars = String.graphemes(row)
      Enum.reduce(Enum.with_index(chars), acc, fn {ch, x}, a ->
        Map.put(a, {x, y}, ch)
      end)
    end)
  end

  def find_robot(m) do
    Enum.find_value(Map.keys(m), nil, fn pos ->
      if Map.get(m, pos) == "@" do
        pos
      else
        nil
      end
    end)
  end

  def add({x, y}, {dx, dy}) do
    {x + dx, y + dy}
  end

  def char_to_dir("^"), do: {0, -1}
  def char_to_dir("<"), do: {-1, 0}
  def char_to_dir(">"), do: {1, 0}
  def char_to_dir("v"), do: {0, 1}
  def char_to_dir(_), do: {0, 0}

  def try_to_step(m, pos, dir) do
    orig = m

    res =
      case Map.get(m, pos) do
        "." -> {:ok, m}
        v when v in ["O", "@"] ->
          new_pos = add(pos, dir)
          case try_to_step(m, new_pos, dir) do
            {:ok, m1} ->
              m2 = Map.put(m1, new_pos, v)
              m3 = Map.put(m2, pos, ".")
              {:ok, m3}
            {:error, _} ->
              {:error, orig}
          end
        "]" ->
          new_pos = add(pos, {-1, 0})
          case try_to_step(m, new_pos, dir) do
            {:ok, m1} -> {:ok, m1}
            {:error, _} -> {:error, orig}
          end
        "[" ->
          case dir do
            {-1, 0} ->
              new_pos = add(pos, {-1, 0})
              case try_to_step(m, new_pos, dir) do
                {:ok, m1} ->
                  m2 = Map.put(m1, new_pos, "[")
                  m3 = Map.put(m2, pos, "]")
                  m4 = Map.put(m3, add(pos, {1, 0}), ".")
                  {:ok, m4}
                {:error, _} -> {:error, orig}
              end
            {1, 0} ->
              pos2 = add(pos, {2, 0})
              case try_to_step(m, pos2, dir) do
                {:ok, m1} ->
                  m2 = Map.put(m1, pos, ".")
                  m3 = Map.put(m2, add(pos, {1, 0}), "[")
                  m4 = Map.put(m3, pos2, "]")
                  {:ok, m4}
                {:error, _} -> {:error, orig}
              end
            _ ->
              pos1 = add(pos, dir)
              pos2 = add(add(pos, dir), {1, 0})
              case try_to_step(m, pos1, dir) do
                {:ok, m1} ->
                  case try_to_step(m1, pos2, dir) do
                    {:ok, m2} ->
                      m3 = Map.put(m2, pos, ".")
                      m4 = Map.put(m3, add(pos, {1, 0}), ".")
                      m5 = Map.put(m4, pos1, "[")
                      m6 = Map.put(m5, pos2, "]")
                      {:ok, m6}
                    {:error, _} -> {:error, orig}
                  end
                {:error, _} -> {:error, orig}
              end
          end
        _ -> {:error, orig}
      end

    case res do
      {:ok, m2} -> {:ok, m2}
      {:error, _} -> {:error, orig}
    end
  end
end

Solver.main([])