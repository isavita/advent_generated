
defmodule Main do
  # ------------------------------------------------------------
  # Entry point
  # ------------------------------------------------------------
  def main do
    lines =
      case File.read("input.txt") do
        {:ok, content} -> String.split(content, "\n", trim: false)
        {:error, _} -> []
      end

    if lines == [] do
      IO.puts("Grand total: 0")
    else
      max_width = lines |> Enum.map(&String.length/1) |> Enum.max()
      is_sep = for x <- 0..(max_width - 1), do: column_all_space?(lines, x)

      total = scan_blocks(lines, is_sep, max_width)
      IO.puts("Grand total: #{total}")
    end
  end

  # ------------------------------------------------------------
  # Is the whole column x only whitespace (or out of range)?
  # ------------------------------------------------------------
  defp column_all_space?(lines, x) do
    Enum.all?(lines, fn line ->
      if x < String.length(line) do
        <<_::binary-size(x), cp::utf8, _::binary>> = line
        <<cp::utf8>> =~ ~r/^\s$/
      else
        true
      end
    end)
  end

  # ------------------------------------------------------------
  # Walk through the columns, extract blocks and sum their results
  # ------------------------------------------------------------
  defp scan_blocks(lines, is_sep, max_width) do
    {total, in_block, start} =
      Enum.reduce(0..(max_width - 1), {0, false, nil}, fn x,
                                                        {total, in_block, start} ->
        if not Enum.at(is_sep, x) do
          # column belongs to a block
          if in_block, do: {total, true, start}, else: {total, true, x}
        else
          # separator column – close a block if we were inside one
          if in_block do
            block_val = process_block(lines, start, x - 1)
            {total + block_val, false, nil}
          else
            {total, false, nil}
          end
        end
      end)

    # last block may reach the right border
    if in_block do
      total + process_block(lines, start, max_width - 1)
    else
      total
    end
  end

  # ------------------------------------------------------------
  # Evaluate a single vertical block (columns start..end inclusive)
  # ------------------------------------------------------------
  defp process_block(lines, start, finish) do
    {op, nums} =
      Enum.reduce(lines, {"", []}, fn line, {op, acc} ->
        if start < String.length(line) do
          seg =
            line
            |> String.slice(start, finish - start + 1)
            |> String.trim()

          cond do
            seg == "" ->
              {op, acc}

            seg == "+" or seg == "*" ->
              {seg, acc}

            true ->
              case Integer.parse(seg) do
                {n, ""} -> {op, [n | acc]}
                _ -> {op, acc}
              end
          end
        else
          {op, acc}
        end
      end)

    nums = Enum.reverse(nums)

    cond do
      nums == [] ->
        0

      op == "+" ->
        Enum.sum(nums)

      op == "*" ->
        Enum.reduce(nums, 1, &*/2)

      length(nums) == 1 ->
        hd(nums)

      true ->
        0
    end
  end
end

Main.main()
