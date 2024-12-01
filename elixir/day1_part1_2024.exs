
defmodule Solution do
  def solve do
    {:ok, file} = File.open("input.txt", [:read])
    lines = IO.read(file, :all) |> String.split("\n", trim: true)
    File.close(file)

    {left, right} = Enum.map(lines, fn line ->
      [left, right] = String.split(line)
      {String.to_integer(left), String.to_integer(right)}
    end) |> Enum.unzip()


    left = Enum.sort(left)
    right = Enum.sort(right)

    Enum.zip(left, right)
    |> Enum.map(fn {l, r} -> abs(l - r) end)
    |> Enum.sum()
    |> IO.puts()
  end
end

Solution.solve()
