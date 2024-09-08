def solve(instructions)
  # Analyze the program
  # The program is essentially calculating a sum of factors of a large number
  
  # Find the target number (usually in one of the last instructions)
  target = instructions.map { |inst| inst.split.map(&:to_i) }.flatten.max

  # Calculate the sum of factors
  sum = 0
  1.upto(Math.sqrt(target).to_i) do |i|
    if target % i == 0
      sum += i
      sum += target / i if i != target / i
    end
  end

  # The result is the sum of factors
  sum
end

# Read input
instructions = File.readlines('input.txt').map(&:chomp)

# Solve and print result
puts solve(instructions)
