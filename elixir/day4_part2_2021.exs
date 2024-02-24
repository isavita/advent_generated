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
    play(drawn_numbers, boards, [], %{})
  end

  defp play([], _boards, _winning_boards, _scores), do: {:error, "No more numbers to draw"}
  defp play([number | rest_numbers], boards, winning_boards, scores) do
    new_boards = Enum.map(boards, &mark_number(&1, number))
    {winners, losers} = Enum.split_with(new_boards, &board_wins?/1)

    updated_scores = Enum.reduce(winners, scores, fn board, acc ->
      if Map.has_key?(acc, :board_id) do
        acc
      else
        board_id = find_board_id(board, boards ++ winning_boards)
        Map.put(acc, board_id, score(board, number))
      end
    end)

    new_winning_boards = winning_boards ++ winners

    if Enum.empty?(losers) and not Enum.empty?(winners) do
      {:ok, Map.values(updated_scores) |> List.last()}
    else
      play(rest_numbers, losers, new_winning_boards, updated_scores)
    end
  end

  defp find_board_id(board, all_boards) do
    Enum.find_index(all_boards, fn b -> b == board end)
  end

  def call do
    {drawn_numbers, boards} = read_input()
    case play(drawn_numbers, boards) do
      {:ok, score} -> IO.puts("Final score of the last winning board: #{score}")
      {:error, _} = error -> IO.inspect(error)
    end
  end
end

Day4.call()
