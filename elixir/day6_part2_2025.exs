
defmodule Solver do
  # -------------------------------------------------
  # entry point
  # -------------------------------------------------
  def main do
    lines =
      File.read!("input.txt")
      |> String.split("\n", trim: false)
      |> Enum.map(&String.trim_trailing(&1, "\r"))

    if lines == [] do
      IO.puts("Grand total: 0")
      exit(:normal)
    end

    line_count = length(lines)
    max_w = lines |> Enum.map(&String.length/1) |> Enum.max()

    # -----------------------------------------------------------------
    # 1) Find separator columns – a column is a separator when every
    #    line has a whitespace (or no character) at that position.
    # -----------------------------------------------------------------
    is_sep =
      for x <- 0..(max_w - 1) do
        Enum.all?(lines, fn line ->
          x >= String.length(line) or
            String.at(line, x) =~ ~r/^\s$/
        end)
      end

    # -----------------------------------------------------------------
    # 2) Detect continuous blocks of non‑separator columns.
    # -----------------------------------------------------------------
    blocks =
      Enum.reduce(0..max_w, {[], false, nil}, fn x,
                                                {acc, in_block, start} ->
        sep = x == max_w or Enum.at(is_sep, x, false)

        cond do
          not sep and not in_block -> {acc, true, x}
          not sep and in_block -> {acc, true, start}
          sep and in_block -> {[{start, x} | acc], false, nil}
          true -> {acc, false, nil}
        end
      end)
      |> elem(0)
      |> Enum.reverse()

    # -----------------------------------------------------------------
    # 3) Evaluate each block and sum the results.
    # -----------------------------------------------------------------
    grand_total =
      Enum.reduce(blocks, 0, fn {s, e}, total ->
        total + block_value(lines, s, e)
      end)

    IO.puts("Grand total: #{grand_total}")
  end

  # -------------------------------------------------
  # Evaluate a single block (columns s..e‑1)
  # -------------------------------------------------
  defp block_value(lines, start_x, end_x) do
    {op, nums} =
      Enum.reduce(start_x..(end_x - 1), {nil, []}, fn col,
                                                    {op_acc, nums_acc} ->
        {num_str, op_acc2} =
          Enum.reduce(lines, {"", op_acc}, fn line,
                                             {sb, cur_op} ->
            if col < String.length(line) do
              ch = String.at(line, col)

              cond do
                ch =~ ~r/^\d$/ -> {sb <> ch, cur_op}
                ch == "+" or ch == "*" -> {sb, ch}
                true -> {sb, cur_op}
              end
            else
              {sb, cur_op}
            end
          end)

        if num_str == "" do
          {op_acc2, nums_acc}
        else
          {op_acc2, [String.to_integer(num_str) | nums_acc]}
        end
      end)

    case op do
      "*" -> Enum.reduce(nums, 1, &*/2)
      _ -> Enum.reduce(nums, 0, &+/2)
    end
  end
end

Solver.main()
