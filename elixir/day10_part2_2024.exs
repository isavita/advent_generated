
defmodule Solver do
  def solve do
    lines =
      "input.txt"
      |> File.read!()
      |> String.trim()
      |> String.split("\n")

    nr = length(lines)
    nc = String.length(hd(lines))

    grid =
      for line <- lines, into: [] do
        for <<char <- line>>, into: [] do
          char - ?0
        end
      end

    dp = :array.new(nr * nc, [{:default, -1}])

    dirs = [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]

    dfs = fn r, c, dfs ->
      index = r * nc + c
      case :array.get(index, dp) do
        -1 ->
          h = Enum.at(grid, r) |> Enum.at(c)

          if h == 9 do
            :array.set(index, 1, dp)
            1
          else
            sum =
              dirs
              |> Enum.reduce(0, fn {dr, dc}, acc ->
                nr2 = r + dr
                nc2 = c + dc

                if nr2 >= 0 and nr2 < nr and nc2 >= 0 and nc2 < nc and
                   Enum.at(grid, nr2) |> Enum.at(nc2) == h + 1 do
                  acc + dfs.(nr2, nc2, dfs)
                else
                  acc
                end
              end)

            :array.set(index, sum, dp)
            sum
          end

        val ->
          val
      end
    end

    total =
      0..(nr - 1)
      |> Enum.reduce(0, fn r, acc_r ->
        0..(nc - 1)
        |> Enum.reduce(acc_r, fn c, acc_c ->
          if Enum.at(grid, r) |> Enum.at(c) == 0 do
            acc_c + dfs.(r, c, dfs)
          else
            acc_c
          end
        end)
      end)

    IO.puts(total)
  end
end

Solver.solve()
