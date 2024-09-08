# Read input
start_positions = File.readlines('input.txt').map { |line| line.split(': ').last.to_i }

# Initialize game state
positions = start_positions.dup
scores = [0, 0]
die = 0
rolls = 0

# Roll die function
def roll_die(die)
  die = die % 100 + 1
  [die, die + 1, die + 2]
end

# Play game
current_player = 0
loop do
  # Roll die and move
  roll = roll_die(die)
  die = (die + 3) % 100
  rolls += 3
  
  # Update position and score
  positions[current_player] = (positions[current_player] + roll.sum - 1) % 10 + 1
  scores[current_player] += positions[current_player]
  
  # Check for win
  if scores[current_player] >= 1000
    losing_score = scores[1 - current_player]
    puts losing_score * rolls
    break
  end
  
  # Switch player
  current_player = 1 - current_player
end
