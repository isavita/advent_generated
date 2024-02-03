
input = File.read("input.txt").split("\n")
initial_state = input.shift.split(": ")[1]
input.shift

rules = input.each_with_object({}) do |line, hash|
  pattern, result = line.split(" => ")
  hash[pattern] = result
end

def evolve(state, rules, generations)
  gen = 0
  offset = 0

  while gen < generations
    state = "....#{state}...."
    offset -= 2
    new_state = ""

    (0..(state.length - 5)).each do |i|
      segment = state[i, 5]
      new_state << (rules[segment] || ".")
    end

    state = new_state.gsub(/^\.+|\.+$/, '')
    offset += new_state.index('#')
    gen += 1
  end

  state.chars.each_with_index.sum { |char, index| char == "#" ? index + offset : 0 }
end

part1 = evolve(initial_state, rules, 20)
puts "Part 1: #{part1}"

# For Part 2, due to the large number of generations, we need to detect a cycle or a stable pattern
# and calculate the sum based on the pattern's shift per generation after a certain point.
# This is a simplified approach for demonstration and might need adjustments based on specific input patterns.

def evolve_with_cycle_detection(state, rules, generations)
  seen_states = {}
  offset = 0
  gen = 0

  while gen < generations
    state_key = state

    if seen_states[state_key]
      cycle_length = gen - seen_states[state_key][:gen]
      remaining_generations = generations - gen
      cycle_steps = remaining_generations / cycle_length
      offset += cycle_steps * (offset - seen_states[state_key][:offset])
      break
    else
      seen_states[state_key] = { gen: gen, offset: offset }
    end

    state = "....#{state}...."
    offset -= 2
    new_state = ""

    (0..(state.length - 5)).each do |i|
      segment = state[i, 5]
      new_state << (rules[segment] || ".")
    end

    state = new_state.gsub(/^\.+|\.+$/, '')
    offset += new_state.index('#')
    gen += 1
  end

  state.chars.each_with_index.sum { |char, index| char == "#" ? index + offset : 0 }
end

part2 = evolve_with_cycle_detection(initial_state, rules, 50000000000)
puts "Part 2: #{part2}"
