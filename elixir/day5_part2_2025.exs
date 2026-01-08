
defmodule Main do
  def main do
    ranges =
      File.stream!("input.txt")
      |> Stream.map(&String.trim/1)
      |> Stream.filter(&(&1 != ""))
      |> Stream.flat_map(fn line ->
        case String.split(line, "-") do
          [a, b] ->
            with {x, ""} <- Integer.parse(a),
                 {y, ""} <- Integer.parse(b) do
              [{min(x, y), max(x, y)}]
            else
              _ -> []
            end

          _ -> []
        end
      end)
      |> Enum.sort_by(fn {l, _} -> l end)

    total =
      case ranges do
        [] ->
          0

        [{s, e} | rest] ->
          {sum, cur_s, cur_e} =
            Enum.reduce(rest, {0, s, e}, fn {s2, e2}, {acc, cs, ce} ->
              if s2 <= ce do
                {acc, cs, max(ce, e2)}
              else
                {acc + (ce - cs + 1), s2, e2}
              end
            end)

          sum + (cur_e - cur_s + 1)
      end

    IO.puts("Total fresh IDs: #{total}")
  end
end

Main.main()
