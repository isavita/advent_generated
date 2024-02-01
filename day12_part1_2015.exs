
defmodule JSAbacusFramework do
  def call do
    "input.txt"
    |> File.read!()
    |> Jason.decode!()
    |> sum_numbers()
  end

  defp sum_numbers(%{} = map), do: Enum.reduce(map, 0, fn {_k, v}, acc -> acc + sum_numbers(v) end)
  defp sum_numbers([head | tail]), do: sum_numbers(head) + sum_numbers(tail)
  defp sum_numbers([]), do: 0
  defp sum_numbers(number) when is_number(number), do: number
  defp sum_numbers(_), do: 0
end
