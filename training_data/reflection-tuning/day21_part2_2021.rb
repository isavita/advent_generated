def play_dirac_dice(p1_pos, p2_pos)
  @memo = {}

  def count_wins(p1_pos, p2_pos, p1_score, p2_score, p1_turn)
    return [1, 0] if p1_score >= 21
    return [0, 1] if p2_score >= 21

    key = [p1_pos, p2_pos, p1_score, p2_score, p1_turn]
    return @memo[key] if @memo.key?(key)

    wins = [0, 0]

    [1, 2, 3].repeated_permutation(3).each do |dice|
      if p1_turn
        new_pos = (p1_pos + dice.sum - 1) % 10 + 1
        new_score = p1_score + new_pos
        sub_wins = count_wins(new_pos, p2_pos, new_score, p2_score, false)
      else
        new_pos = (p2_pos + dice.sum - 1) % 10 + 1
        new_score = p2_score + new_pos
        sub_wins = count_wins(p1_pos, new_pos, p1_score, new_score, true)
      end
      wins[0] += sub_wins[0]
      wins[1] += sub_wins[1]
    end

    @memo[key] = wins
    wins
  end

  wins = count_wins(p1_pos, p2_pos, 0, 0, true)
  wins.max
end

# Read input from file
p1_pos, p2_pos = File.readlines('input.txt').map { |line| line.split.last.to_i }

# Calculate and print the result
puts play_dirac_dice(p1_pos, p2_pos)
