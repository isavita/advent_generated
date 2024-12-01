
defmodule Solution do
  def solve do
    {:ok, file} = File.open("input.txt", [:read])
    lines = IO.read(file, :all) |> String.split("\n", trim: true)
    File.close(file)

    {left, right} = Enum.map(lines, fn line ->
      [left, right] = String.split(line)
      {String.to_integer(left), String.to_integer(right)}
    end) |> Enum.unzip()


    right_count = Enum.reduce(right, %{}, fn num, acc ->
      Map.update(acc, num, 1, &(&1 + 1))
    end)

    Enum.reduce(left, 0, fn num, acc ->
      case Map.get(right_count, num) do
        nil -> acc
        count -> acc + num * count
      end
    end)
  end
end

IO.puts Solution.solve()
