
defmodule Solution do
  defp parse_seeds(line) do
    line
    |> String.split(" ")
    |> Enum.chunk_every(2)
    |> Enum.map(fn [start, len] ->
      {String.to_integer(start), String.to_integer(len)}
    end)
  end

  defp parse_map_line(line) do
    case String.split(line, " ") do
      [dest, src, len] ->
        {String.to_integer(src), String.to_integer(dest), String.to_integer(len)}

      _ ->
        nil
    end
  end

  defp parse_input(lines) do
    [seeds_line | rest] = lines
    seeds = parse_seeds(String.replace(seeds_line, "seeds: ", ""))
    maps = do_parse_maps(rest, [], [])
    {seeds, maps}
  end

  defp do_parse_maps([], acc, curr) do
    Enum.reverse([curr | acc])
  end

  defp do_parse_maps([line | rest], acc, curr) do
    if String.contains?(line, "map:") do
      do_parse_maps(rest, [curr | acc], [])
    else
      case parse_map_line(line) do
        nil -> do_parse_maps(rest, acc, curr)
        {src, dest, len} -> do_parse_maps(rest, acc, [%{src: src, dest: dest, len: len} | curr])
      end
    end
  end

  defp reverse_convert(number, ranges) do
    Enum.reduce(ranges, number, fn %{src: src, dest: dest, len: len}, acc ->
      if acc >= dest && acc < dest + len do
        src + (acc - dest)
      else
        acc
      end
    end)
  end

  defp in_seed_range?(number, ranges) do
    Enum.any?(ranges, fn {start, len} ->
      number >= start && number < start + len
    end)
  end

  def solve() do
    lines = File.read!("input.txt") |> String.split("\n", trim: true)
    {seeds, maps} = parse_input(lines)

    Stream.iterate(0, &(&1 + 1))
    |> Enum.find(fn location ->
      seed = Enum.reduce(Enum.reverse(maps), location, &reverse_convert(&2, &1))
      in_seed_range?(seed, seeds)
    end)
    |> IO.puts()
  end
end

Solution.solve()
