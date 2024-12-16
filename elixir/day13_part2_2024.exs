
defmodule Solution do
  def solve do
    "input.txt"
    |> read_input()
    |> Enum.map(&solve_machine/1)
    |> Enum.filter(&(&1 >= 0))
    |> case do
      [] -> IO.puts("0 0")
      results ->
        count = length(results)
        sum = Enum.sum(results)
        IO.puts("#{count} #{sum}")
    end
  end

  defp read_input(filename) do
    File.read!(filename)
    |> String.split("\n\n", trim: true)
    |> Enum.map(&parse_machine/1)
    |> Enum.map(fn machine ->
      %{machine | px: machine.px + 1_000_000_000_000_0, py: machine.py + 1_000_000_000_000_0}
    end)
  end

  defp parse_machine(lines) do
    lines
    |> String.split("\n", trim: true)
    |> Enum.reduce(%{ax: 0, ay: 0, bx: 0, by: 0, px: 0, py: 0}, fn line, acc ->
      cond do
        String.starts_with?(line, "Button A:") ->
          {ax, ay} = parse_line(String.slice(line, 9..-1))
          %{acc | ax: ax, ay: ay}
        String.starts_with?(line, "Button B:") ->
          {bx, by} = parse_line(String.slice(line, 9..-1))
          %{acc | bx: bx, by: by}
        String.starts_with?(line, "Prize:") ->
          {px, py} = parse_prize(String.slice(line, 6..-1))
          %{acc | px: px, py: py}
        true ->
          acc
      end
    end)
  end

  defp parse_line(s) do
    [x, y] = String.split(s, ",", trim: true)
    {parse_val(x), parse_val(y)}
  end

  defp parse_prize(s) do
    [x, y] = String.split(s, ",", trim: true)
    {parse_val_prize(x), parse_val_prize(y)}
  end

  defp parse_val(s) do
    s
    |> String.trim()
    |> String.replace_prefix("X+", "")
    |> String.replace_prefix("Y+", "")
    |> String.replace_prefix("X=", "")
    |> String.replace_prefix("Y=", "")
    |> String.to_integer()
  end

  defp parse_val_prize(s) do
    s
    |> String.trim()
    |> String.replace_prefix("X=", "")
    |> String.replace_prefix("Y=", "")
    |> String.to_integer()
  end

  defp solve_machine(%{ax: ax, ay: ay, bx: bx, by: by, px: px, py: py}) do
    d = ax * by - ay * bx
    if d == 0 do
      -1
    else
      num_a = px * by - py * bx
      num_b = -px * ay + py * ax
      if rem(num_a, d) != 0 or rem(num_b, d) != 0 do
        -1
      else
        a = div(num_a, d)
        b = div(num_b, d)
        if a < 0 or b < 0 do
          -1
        else
          3 * a + b
        end
      end
    end
  end
end

Solution.solve()
