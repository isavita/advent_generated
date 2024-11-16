
def parse_requirements(lines)
  requirements = Hash(Char, Set(Char)).new { |h, k| h[k] = Set(Char).new }
  lines.each do |line|
    parts = line.split
    prerequisite = parts[1][0]
    step = parts[7][0]
    requirements[step].add(prerequisite)
    requirements[prerequisite] ||= Set(Char).new
  end
  requirements
end

def find_step_order(requirements)
  completed = Set(Char).new
  order = [] of Char

  while !requirements.empty?
    # Find available steps (no remaining prerequisites)
    available_steps = requirements.select do |step, prereqs|
      prereqs.empty?
    end.keys.to_a.sort

    if available_steps.empty?
      break
    end

    # Choose first alphabetically
    next_step = available_steps.first

    # Add step to order and mark as completed
    order << next_step
    completed.add(next_step)

    # Remove the step from requirements
    requirements.delete(next_step)

    # Remove completed step from other steps' prerequisites
    requirements.each_value do |prereqs|
      prereqs.delete(next_step)
    end
  end

  order.join
end

# Read input from file
input = File.read_lines("input.txt")

# Solve the problem
requirements = parse_requirements(input)
result = find_step_order(requirements)

# Print the result
puts result
