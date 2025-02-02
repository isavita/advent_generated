
defmodule Solution do
  def is_prime(n) do
    2..trunc(:math.sqrt(n))
    |> Enum.all?(fn i -> rem(n, i) != 0 end)
  end

  def solve do
    b = 57 * 100 + 100000
    c = b + 17000
    b..c//17
    |> Enum.reject(&is_prime/1)
    |> length()
  end
end

Solution.solve() |> IO.puts()
