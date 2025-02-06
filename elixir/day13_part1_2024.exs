
defmodule Solver do
  def solve(filename) do
    filename
    |> read_input()
    |> Enum.reduce({0, 0}, fn m, {count, sum} ->
      case solve_machine(m) do
        cost when cost >= 0 -> {count + 1, sum + cost}
        _ -> {count, sum}
      end
    end)
    |> print_result()
  end

  defp read_input(filename) do
    File.read!(filename)
    |> String.split("\n\n", trim: true)
    |> Enum.map(&parse_machine/1)
  end

  defp parse_machine(machine_str) do
    lines = String.split(machine_str, "\n", trim: true)

    parse_coords = fn line ->
      line
      |> String.replace(~r/Button [AB]:/, "")
      |> String.replace("Prize:", "")
      |> String.trim()
      |> String.split(",", trim: true)
      |> Enum.map(&parse_val/1)
    end

    %{
      a: lines |> Enum.find(&String.starts_with?(&1, "Button A:")) |> parse_coords.(),
      b: lines |> Enum.find(&String.starts_with?(&1, "Button B:")) |> parse_coords.(),
      p: lines |> Enum.find(&String.starts_with?(&1, "Prize:")) |> parse_coords.()
    }
  end

  defp parse_val(s) do
    s
    |> String.trim()
    |> String.replace(~r/[XY][+=]/, "")
    |> String.to_integer()
  end

  defp solve_machine(%{a: [ax, ay], b: [bx, by], p: [px, py]}) do
    0..100
    |> Enum.reduce(-1, fn a_count, min_cost ->
      0..100
      |> Enum.reduce(min_cost, fn b_count, acc ->
        x = ax * a_count + bx * b_count
        y = ay * a_count + by * b_count

        if x == px && y == py do
          cost = a_count * 3 + b_count
          if acc < 0 || cost < acc, do: cost, else: acc
        else
          acc
        end
      end)
    end)
  end

  defp print_result({0, 0}), do: IO.puts("0 0")

  defp print_result({count, sum}), do: IO.puts("#{count} #{sum}")
end

Solver.solve("input.txt")
