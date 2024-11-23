
defmodule DiceGame do
  def solve do
    {:ok, input} = File.read("input.txt")
    [player1_start, player2_start] = 
      input 
      |> String.split("\n")
      |> Enum.map(fn line -> 
        line 
        |> String.slice(28..-1) 
        |> String.trim() 
        |> String.to_integer() 
      end)

    play(player1_start, 0, player2_start, 0, 1, 0)
  end

  defp play(p1_pos, p1_score, p2_pos, p2_score, die_roll, roll_count) do
    # Player 1 turn
    {new_p1_pos, new_p1_score, new_die_roll, new_roll_count} = 
      take_turn(p1_pos, p1_score, die_roll, roll_count)

    if new_p1_score >= 1000 do
      p2_score * new_roll_count
    else
      # Player 2 turn
      {new_p2_pos, new_p2_score, final_die_roll, final_roll_count} = 
        take_turn(p2_pos, p2_score, new_die_roll, new_roll_count)

      if new_p2_score >= 1000 do
        new_p1_score * final_roll_count
      else
        play(new_p1_pos, new_p1_score, new_p2_pos, new_p2_score, final_die_roll, final_roll_count)
      end
    end
  end

  defp take_turn(pos, score, die_roll, roll_count) do
    rolls = 
      [die_roll, die_roll + 1, die_roll + 2]
      |> Enum.map(fn r -> rem(r - 1, 100) + 1 end)
      |> Enum.sum()

    new_pos = rem(pos + rolls - 1, 10) + 1
    new_score = score + new_pos

    {new_pos, new_score, die_roll + 3, roll_count + 3}
  end

  def run do
    solve() |> IO.puts()
  end
end

DiceGame.run()
