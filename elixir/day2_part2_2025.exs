
defmodule Main do
  @spec is_invalid(integer) :: boolean
  defp is_invalid(x) do
    s = Integer.to_string(x)
    n = byte_size(s)
    if n <= 1, do: false, else: check(s, n, 1)
  end

  @spec check(binary, integer, integer) :: boolean
  defp check(_s, n, p) when p > div(n, 2), do: false
  defp check(s, n, p) do
    if rem(n, p) != 0 do
      check(s, n, p + 1)
    else
      k = div(n, p)
      if k < 2 do
        check(s, n, p + 1)
      else
        chunk = binary_part(s, 0, p)
        if repeats?(s, chunk, p, n) do
          true
        else
          check(s, n, p + 1)
        end
      end
    end
  end

  @spec repeats?(binary, binary, integer, integer) :: boolean
  defp repeats?(_s, _chunk, p, n) when p >= n, do: true
  defp repeats?(s, chunk, p, n) do
    case binary_part(s, p, byte_size(chunk)) do
      ^chunk -> repeats?(s, chunk, p + byte_size(chunk), n)
      _ -> false
    end
  end

  @spec process_range(integer, integer, integer) :: integer
  defp process_range(a, b, sum) when a > b, do: sum
  defp process_range(a, b, sum) do
    if is_invalid(a) do
      process_range(a + 1, b, sum + a)
    else
      process_range(a + 1, b, sum)
    end
  end

  def main do
    data = File.read!("input.txt")
    sum =
      Regex.scan(~r/(\d+)-(\d+)/, data)
      |> Enum.reduce(0, fn [_, a_str, b_str], acc ->
        a = String.to_integer(a_str)
        b = String.to_integer(b_str)
        {lo, hi} = if a <= b, do: {a, b}, else: {b, a}
        acc + process_range(lo, hi, 0)
      end)
    IO.puts(sum)
  end
end

Main.main()
