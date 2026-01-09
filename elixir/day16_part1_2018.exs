
defmodule Solution do
  import Bitwise

  def main do
    input = File.read!("input.txt")
    samples_part = input |> String.split("\n\n\n") |> List.first()
    samples = String.split(samples_part, "\n\n", trim: true)

    ops = [
      fn a, b, c, r -> put_elem(r, c, elem(r, a) + elem(r, b)) end,
      fn a, b, c, r -> put_elem(r, c, elem(r, a) + b) end,
      fn a, b, c, r -> put_elem(r, c, elem(r, a) * elem(r, b)) end,
      fn a, b, c, r -> put_elem(r, c, elem(r, a) * b) end,
      fn a, b, c, r -> put_elem(r, c, band(elem(r, a), elem(r, b))) end,
      fn a, b, c, r -> put_elem(r, c, band(elem(r, a), b)) end,
      fn a, b, c, r -> put_elem(r, c, bor(elem(r, a), elem(r, b))) end,
      fn a, b, c, r -> put_elem(r, c, bor(elem(r, a), b)) end,
      fn a, _, c, r -> put_elem(r, c, elem(r, a)) end,
      fn a, _, c, r -> put_elem(r, c, a) end,
      fn a, b, c, r -> put_elem(r, c, if(a > elem(r, b), do: 1, else: 0)) end,
      fn a, b, c, r -> put_elem(r, c, if(elem(r, a) > b, do: 1, else: 0)) end,
      fn a, b, c, r -> put_elem(r, c, if(elem(r, a) > elem(r, b), do: 1, else: 0)) end,
      fn a, b, c, r -> put_elem(r, c, if(a == elem(r, b), do: 1, else: 0)) end,
      fn a, b, c, r -> put_elem(r, c, if(elem(r, a) == b, do: 1, else: 0)) end,
      fn a, b, c, r -> put_elem(r, c, if(elem(r, a) == elem(r, b), do: 1, else: 0)) end
    ]

    Enum.count(samples, fn s ->
      case String.split(s, "\n", trim: true) do
        [b_s, i_s, a_s] ->
          rb = parse(b_s) |> List.to_tuple()
          ra = parse(a_s) |> List.to_tuple()
          [_, a, b, c] = parse(i_s)
          Enum.count(ops, fn op -> op.(a, b, c, rb) == ra end) >= 3
        _ -> false
      end
    end)
    |> IO.puts()
  end

  defp parse(s) do
    Regex.scan(~r/\d+/, s) |> List.flatten() |> Enum.map(&String.to_integer/1)
  end
end

Solution.main()

