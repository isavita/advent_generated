
defmodule Main do
  def main do
    input = File.read!("input.txt") |> String.trim() |> String.to_integer()
    target = input + 10
    board = :array.from_list([3, 7])
    loop(board, 2, 0, 1, target, input)
  end

  defp loop(board, size, e1, e2, target, start) when size >= target do
    result =
      for i <- start..(start + 9) do
        :array.get(i, board)
      end
      |> Enum.join()
    IO.puts(result)
  end

  defp loop(board, size, e1, e2, target, start) do
    sum = :array.get(e1, board) + :array.get(e2, board)

    {board, size} =
      if sum >= 10 do
        board = :array.set(size, 1, board)
        board = :array.set(size + 1, rem(sum, 10), board)
        {board, size + 2}
      else
        board = :array.set(size, sum, board)
        {board, size + 1}
      end

    e1 = rem(e1 + 1 + :array.get(e1, board), size)
    e2 = rem(e2 + 1 + :array.get(e2, board), size)

    loop(board, size, e1, e2, target, start)
  end
end

Main.main()
