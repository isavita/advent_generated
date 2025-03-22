
defmodule DiracDice do
  def read_input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      String.split(line, ": ")
      |> List.last()
      |> String.to_integer()
    end)
  end

  def part1(player1_start, player2_start) do
    play_deterministic(player1_start, player2_start)
  end

  def play_deterministic(player1_start, player2_start) do
    {loser_score, die_rolls} =
      do_play_deterministic(player1_start, player2_start, 0, 0, 1, true)

    loser_score * die_rolls
  end

  defp do_play_deterministic(
         player1_pos,
         player2_pos,
         player1_score,
         player2_score,
         die_roll,
         player1_turn,
         die_rolls \\ 0
       ) do
    cond do
      player1_score >= 1000 -> {player2_score, die_rolls}
      player2_score >= 1000 -> {player1_score, die_rolls}
      true ->
        {roll1, die_roll} = {die_roll, rem(die_roll, 100) + 1}
        {roll2, die_roll} = {die_roll, rem(die_roll, 100) + 1}
        {roll3, die_roll} = {die_roll, rem(die_roll, 100) + 1}
        roll_sum = roll1 + roll2 + roll3

        if player1_turn do
          new_pos = rem(player1_pos + roll_sum - 1, 10) + 1
          new_score = player1_score + new_pos

          do_play_deterministic(
            new_pos,
            player2_pos,
            new_score,
            player2_score,
            die_roll,
            false,
            die_rolls + 3
          )
        else
          new_pos = rem(player2_pos + roll_sum - 1, 10) + 1
          new_score = player2_score + new_pos

          do_play_deterministic(
            player1_pos,
            new_pos,
            player1_score,
            new_score,
            die_roll,
            true,
            die_rolls + 3
          )
        end
    end
  end

  def part2(player1_start, player2_start) do
    {p1_wins, p2_wins} = play_quantum(player1_start, player2_start)

    max(p1_wins, p2_wins)
  end

  @roll_counts %{
    3 => 1,
    4 => 3,
    5 => 6,
    6 => 7,
    7 => 6,
    8 => 3,
    9 => 1
  }

  def play_quantum(player1_start, player2_start) do
    memo = %{}
    quantum_play(player1_start, 0, player2_start, 0, true, 1, memo)
  end

  defp quantum_play(
         p1_pos,
         p1_score,
         p2_pos,
         p2_score,
         player1_turn,
         universes,
         memo
       ) do
    key = {p1_pos, p1_score, p2_pos, p2_score, player1_turn}

    case Map.get(memo, key) do
      {:ok, result} ->
        result

      nil ->
        cond do
          p1_score >= 21 ->
            {{universes, 0}, memo}

          p2_score >= 21 ->
            {{0, universes}, memo}

          true ->
            wins =
              @roll_counts
              |> Enum.reduce({0, 0}, fn {roll, count}, {p1_total, p2_total} ->
                new_universes = universes * count

                if player1_turn do
                  new_pos = rem(p1_pos + roll - 1, 10) + 1
                  new_score = p1_score + new_pos

                  {{p1_wins, p2_wins}, _} =
                    quantum_play(
                      new_pos,
                      new_score,
                      p2_pos,
                      p2_score,
                      false,
                      new_universes,
                      memo
                    )

                  {p1_total + p1_wins, p2_total + p2_wins}
                else
                  new_pos = rem(p2_pos + roll - 1, 10) + 1
                  new_score = p2_score + new_pos

                  {{p1_wins, p2_wins}, _} =
                    quantum_play(
                      p1_pos,
                      p1_score,
                      new_pos,
                      new_score,
                      true,
                      new_universes,
                      memo
                    )

                  {p1_total + p1_wins, p2_total + p2_wins}
                end
              end)

            result = {wins, Map.put(memo, key, {:ok, wins})}
            result
        end
    end
  end
end

# Main execution
filename = "input.txt"
[player1_start, player2_start] = DiracDice.read_input(filename)

IO.puts("Part 1: #{DiracDice.part1(player1_start, player2_start)}")
IO.puts("Part 2: #{DiracDice.part2(player1_start, player2_start)}")
