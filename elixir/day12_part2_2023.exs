defmodule Row do
  defstruct springs: "", group: [], group_len: 0
end

defmodule Solver do
  def parse_row(line) do
    [springs, group_str] = String.split(line, " ", parts: 2)
    group = String.split(group_str, ",") |> Enum.map(&String.to_integer/1)
    %Row{springs: springs, group: group, group_len: length(group)}
  end

  def unfold_row(%Row{} = row, factor) do
    new_springs =
      Enum.reduce(0..(factor - 1), "", fn i, acc ->
        acc <> row.springs <> (if i < factor - 1, do: "?", else: "")
      end)

    new_group = List.flatten(Enum.map(1..factor, fn _ -> row.group end))
    %Row{springs: new_springs, group: new_group, group_len: length(new_group)}
  end

  def count_arrangements(%Row{} = row) do
    {value, _memo} = do_count(row, 0, 0, 0, %{})
    value
  end

  defp do_count(%Row{} = row, iSprings, iGroup, iCont, memo) do
    len = String.length(row.springs)

    if iSprings == len do
      value =
        if (iGroup == row.group_len and iCont == 0) or
             (iGroup == row.group_len - 1 and iCont == Enum.at(row.group, iGroup)) do
          1
        else
          0
        end

      {value, memo}
    else
      key = {iSprings, iGroup, iCont}

      case Map.fetch(memo, key) do
        {:ok, value} -> {value, memo}
        :error ->
          char = String.at(row.springs, iSprings)

          {res1, memo1} =
            if char in [".", "?"] do
              if iCont == 0 do
                do_count(row, iSprings + 1, iGroup, iCont, memo)
              else
                if iGroup < row.group_len and iCont == Enum.at(row.group, iGroup) do
                  do_count(row, iSprings + 1, iGroup + 1, 0, memo)
                else
                  {0, memo}
                end
              end
            else
              {0, memo}
            end

          {res2, memo2} =
            if char in ["#", "?"] do
              if iGroup < row.group_len and iCont < Enum.at(row.group, iGroup) do
                do_count(row, iSprings + 1, iGroup, iCont + 1, memo1)
              else
                {0, memo1}
              end
            else
              {0, memo1}
            end

          value = res1 + res2
          memo3 = Map.put(memo2, key, value)
          {value, memo3}
      end
    end
  end
end

defmodule Main do
  def main do
    total =
      File.stream!("input.txt", []) 
      |> Enum.reduce(0, fn line, acc ->
        line = String.trim(line)
        if line == "" do
          acc
        else
          row = Solver.parse_row(line)
          unfolded = Solver.unfold_row(row, 5)
          acc + Solver.count_arrangements(unfolded)
        end
      end)

    IO.puts(total)
  end
end

Main.main()