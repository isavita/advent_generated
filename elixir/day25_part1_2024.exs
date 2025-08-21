
defmodule Main do
  def main do
    lines =
      "input.txt"
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.map(&String.trim/1)
      |> Enum.filter(&(&1 != ""))

    if rem(length(lines), 7) != 0 do
      IO.puts("0")
      exit(:normal)
    end

    {locks, keys} =
      lines
      |> Enum.chunk_every(7)
      |> Enum.reduce({[], []}, fn block, {ls, ks} ->
        if Enum.all?(block, fn l -> String.length(l) >= 5 end) do
          if all_char?(hd(block), "#") do
            {[parse_lock(block) | ls], ks}
          else
            {ls, [parse_key(block) | ks]}
          end
        else
          {ls, ks}
        end
      end)

    total =
      Enum.reduce(locks, 0, fn lock, acc ->
        acc + Enum.count(keys, fn key -> fits?(lock, key) end)
      end)

    IO.puts(total)
  end

  defp all_char?(s, ch) do
    String.graphemes(s) |> Enum.all?(&(&1 == ch))
  end

  defp parse_lock(block) do
    for c <- 0..4 do
      Enum.reduce_while(1..6, 0, fn r, cnt ->
        if String.at(Enum.at(block, r), c) == "#" do
          {:cont, cnt + 1}
        else
          {:halt, cnt}
        end
      end)
    end
  end

  defp parse_key(block) do
    for c <- 0..4 do
      Enum.reduce_while(0..5, 0, fn i, cnt ->
        r = 5 - i
        if String.at(Enum.at(block, r), c) == "#" do
          {:cont, cnt + 1}
        else
          {:halt, cnt}
        end
      end)
    end
  end

  defp fits?(lock, key) do
    Enum.zip(lock, key) |> Enum.all?(fn {l, k} -> l + k <= 5 end)
  end
end

Main.main()
