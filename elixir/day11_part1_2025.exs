
defmodule Main do
  def main do
    adj =
      "input.txt"
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.reduce(%{}, fn line, acc ->
        [src, dests] = String.split(line, ":", parts: 2)
        dest_list = String.split(String.trim(dests))
        Map.put(acc, String.trim(src), dest_list)
      end)

    count = count_paths("you", "out", adj, %{})
    IO.puts(count)
  end

  defp count_paths(cur, target, adj, memo) do
    cond do
      cur == target -> {1, memo}
      Map.has_key?(memo, cur) -> {Map.get(memo, cur), memo}
      true ->
        {sum, new_memo} =
          adj
          |> Map.get(cur, [])
          |> Enum.reduce({0, memo}, fn nxt, {acc, m} ->
            {c, m2} = count_paths(nxt, target, adj, m)
            {acc + c, m2}
          end)

        {sum, Map.put(new_memo, cur, sum)}
    end
  end
end

Main.main()
