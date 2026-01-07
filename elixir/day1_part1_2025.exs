
defmodule Main do
  def main do
    {cnt, _} =
      File.stream!("input.txt", [], :line)
      |> Enum.reduce({0, 50}, fn line, {cnt, pos} ->
        line = String.trim(line)
        {dir, rest} = String.split_at(line, 1)
        amt = String.to_integer(rest)
        pos = rem(pos + (if dir == "R", do: amt, else: -amt), 100)
        pos = if pos < 0, do: pos + 100, else: pos
        {cnt + (if pos == 0, do: 1, else: 0), pos}
      end)
    IO.puts(cnt)
  end
end

Main.main()
