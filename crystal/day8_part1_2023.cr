
def parse_input(input_lines)
  instructions = input_lines[0].chars
  network = {} of String => {left: String, right: String}

  input_lines[2..-1].each do |line|
    match = line.match(/(\w+) = \((\w+), (\w+)\)/)
    if match
      node = match[1]
      left = match[2]
      right = match[3]
      network[node] = {left: left, right: right}
    end
  end

  {instructions, network}
end

def navigate_network(instructions, network)
  current_node = "AAA"
  steps = 0
  instruction_length = instructions.size

  while current_node != "ZZZ"
    instruction = instructions[steps % instruction_length]
    current_node = instruction == 'L' ? network[current_node][:left] : network[current_node][:right]
    steps += 1
  end

  steps
end

# Read input from file
input_lines = File.read_lines("input.txt")

# Parse input and navigate network
instructions, network = parse_input(input_lines)
total_steps = navigate_network(instructions, network)

# Print result
puts total_steps
