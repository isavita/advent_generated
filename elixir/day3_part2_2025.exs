
defmodule Main do
  @k 12

  # entry point ---------------------------------------------------------------
  def main do
    total =
      "input.txt"
      |> File.stream!()
      |> Stream.map(&String.trim/1)
      |> Stream.filter(&valid_line?/1)
      |> Enum.reduce(0, fn line, acc ->
        best = max_subseq(line, @k)
        acc + String.to_integer(best)
      end)

    IO.puts("Total output joltage: #{total}")
  end

  # ---------------------------------------------------------------------------
  defp valid_line?(s), do: s != "" and String.length(s) >= @k

  # ---------------------------------------------------------------------------
  # Returns the maximum numeric subsequence of length k from the string s.
  # Implements the monotonic‑stack algorithm in O(n) time.
  defp max_subseq(s, k) do
    to_remove = String.length(s) - k

    {stack, _} =
      s
      |> String.graphemes()
      |> Enum.reduce({[], to_remove}, fn d, {stk, rem} ->
        {new_stk, new_rem} = pop_while(stk, rem, d)
        {[d | new_stk], new_rem}
      end)

    stack
    |> Enum.reverse()
    |> Enum.take(k)
    |> Enum.join()
  end

  # Pops from the stack while the top element is smaller than the current digit
  # and we still have removals left.
  defp pop_while([h | t] = stk, rem, d) when rem > 0 and h < d do
    pop_while(t, rem - 1, d)
  end

  defp pop_while(stk, rem, _d), do: {stk, rem}
end

# Run the program -------------------------------------------------------------
Main.main()
