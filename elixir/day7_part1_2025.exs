
defmodule Main do
  def main do
    lines =
      "input.txt"
      |> File.read!()
      |> String.split("\n", trim: true)

    if lines == [] do
      IO.puts("Empty grid")
      System.halt(0)
    end

    {start_x, start_y} =
      lines
      |> Enum.with_index()
      |> Enum.reduce_while({-1, -1}, fn {row, y}, _acc ->
        case :binary.match(row, "S") do
          {x, _} -> {:halt, {x, y}}
          :nomatch -> {:cont, nil}
        end
      end)

    width = String.length(List.first(lines))

    {_, splits} =
      Enum.reduce(start_y..(length(lines) - 1), {MapSet.new([start_x]), 0}, fn y,
                                                                            {active, splits} ->
        {next, inc} =
          Enum.reduce(active, {MapSet.new(), 0}, fn x, {acc, inc} ->
            cond do
              x < 0 or x >= width -> {acc, inc}
              true ->
                case String.at(Enum.at(lines, y), x) do
                  "^" ->
                    acc =
                      acc
                      |> maybe_add(x - 1, width)
                      |> maybe_add(x + 1, width)

                    {acc, inc + 1}

                  _ ->
                    {MapSet.put(acc, x), inc}
                end
            end
          end)

        {next, splits + inc}
      end)

    IO.puts("Total times the beam is split: #{splits}")
  end

  defp maybe_add(set, x, width) when x >= 0 and x < width, do: MapSet.put(set, x)
  defp maybe_add(set, _x, _width), do: set
end

Main.main()
