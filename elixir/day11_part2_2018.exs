
defmodule Main do
  @grid_size 300

  defp power(x, y, serial) do
    rack = x + 10
    power = rack * y
    power = power + serial
    power = power * rack
    digit = rem(div(power, 100), 10)
    digit - 5
  end

  def main do
    serial = File.read!("input.txt") |> String.trim() |> String.to_integer()
    size = @grid_size

    grid =
      for y <- 1..size, x <- 1..size, into: %{} do
        {{y, x}, power(x, y, serial)}
      end

    sum =
      for y <- 0..size, x <- 0..size, into: %{} do
        {{y, x}, 0}
      end

    sum =
      Enum.reduce(1..size, sum, fn y, acc_y ->
        Enum.reduce(1..size, acc_y, fn x, acc_x ->
          val = grid[{y, x}]
          s = val + acc_x[{y - 1, x}] + acc_x[{y, x - 1}] - acc_x[{y - 1, x - 1}]
          Map.put(acc_x, {y, x}, s)
        end)
      end)

    max = %{power: -1_000_000_000, x: 0, y: 0, size: 0}

    max =
      Enum.reduce(1..size, max, fn sz, max_sz ->
        Enum.reduce(1..(size - sz + 1), max_sz, fn y, max_y ->
          Enum.reduce(1..(size - sz + 1), max_y, fn x, max_x ->
            total =
              sum[{y + sz - 1, x + sz - 1}] -
                sum[{y - 1, x + sz - 1}] -
                sum[{y + sz - 1, x - 1}] +
                sum[{y - 1, x - 1}]

            if total > max_x.power do
              %{power: total, x: x, y: y, size: sz}
            else
              max_x
            end
          end)
        end)
      end)

    IO.puts("#{max.x},#{max.y},#{max.size}")
  end
end

Main.main()
