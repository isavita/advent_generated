defmodule Day4 do
  def read_input do
    input = File.read!("input.txt") |> String.trim()
    [drawn_numbers_line | boards_lines] = String.split(input, "\n\n", trim: true)

    drawn_numbers = drawn_numbers_line
                    |> String.split(",", trim: true)
                    |> Enum.map(&String.to_integer/1)

    boards = for board <- boards_lines do
      board
      |> String.split("\n")
      |> Enum.map(fn line ->
        line
        |> String.split(~r/\s+/, trim: true)
        |> Enum.map(&String.to_integer/1)
      end)
    end

    {drawn_numbers, boards}
  end 

  defp mark_number(board, number) do
    Enum.map(board, fn row ->
      Enum.map(row, fn n -> if n == number, do: :marked, else: n end)
    end)
  end

  defp board_wins?(board) do
    Enum.any?(board, fn row -> Enum.all?(row, &(&1 == :marked)) end) or
      Enum.any?(0..Enum.count(board) - 1, fn i ->
        Enum.all?(board, fn row -> Enum.at(row, i) == :marked end)
      end)
  end

  defp score(board, last_number) do
    Enum.flat_map(board, fn row ->
      Enum.filter(row, fn n -> n != :marked end)
    end)
    |> Enum.sum()
    |> Kernel.*(last_number)
  end

  def play(drawn_numbers, boards) do
    play(drawn_numbers, boards, [])
  end

  defp play([], _boards, _marked_boards), do: {:error, "No winner"}
  defp play([number | rest_numbers], boards, marked_boards) do
    new_marked_boards = Enum.map(boards ++ marked_boards, &mark_number(&1, number))

    case Enum.find(new_marked_boards, &board_wins?(&1)) do
      nil -> play(rest_numbers, [], new_marked_boards)
      winning_board -> {:ok, score(winning_board, number)}
    end
  end

  def call do
    {drawn_numbers, boards} = read_input()
    case play(drawn_numbers, boards) do
      {:ok, score} -> IO.puts("Winning score: #{score}")
      {:error, _} = error -> IO.inspect(error)
    end
  end
end

Day4.call()

