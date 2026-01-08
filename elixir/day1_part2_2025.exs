
defmodule Main do
  @dial_size 100

  defp floor_div(a, b) do
    q = div(a, b)
    r = rem(a, b)

    if r != 0 and ((a < 0) != (b < 0)), do: q - 1, else: q
  end

  def main do
    {_, total_zero_hits} =
      File.stream!("input.txt")
      |> Stream.map(&String.trim/1)
      |> Stream.filter(&(&1 != ""))
      |> Enum.reduce({50, 0}, fn line, {pos, hits} ->
        direction = String.at(line, 0)
        amount = String.slice(line, 1..-1) |> String.to_integer()

        case direction do
          "R" ->
            new_hits = hits + div(pos + amount, @dial_size)
            new_pos = rem(pos + amount, @dial_size)
            {new_pos, new_hits}

          "L" ->
            delta =
              floor_div(pos - 1, @dial_size) -
                floor_div(pos - amount - 1, @dial_size)

            new_hits = hits + delta
            new_pos = rem(pos - amount, @dial_size)
            new_pos = if new_pos < 0, do: new_pos + @dial_size, else: new_pos
            {new_pos, new_hits}
        end
      end)

    IO.puts("The password is: #{total_zero_hits}")
  end
end

Main.main()
