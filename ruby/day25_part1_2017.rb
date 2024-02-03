
def parse_input(file_path)
  lines = File.readlines(file_path).map(&:chomp)
  
  initial_state = lines[0][-2]
  steps = lines[1].scan(/\d+/).first.to_i
  
  states = {}
  (3..lines.length-1).step(10) do |i|
    state = lines[i][-2]
    value0 = lines[i+2][-2].to_i
    move0 = lines[i+3].end_with?("left.") ? -1 : 1
    next_state0 = lines[i+4][-2]
    value1 = lines[i+6][-2].to_i
    move1 = lines[i+7].end_with?("left.") ? -1 : 1
    next_state1 = lines[i+8][-2]
    states[state] = { 0 => [value0, move0, next_state0], 1 => [value1, move1, next_state1] }
  end
  
  [initial_state, steps, states]
end

def run_turing_machine(file_path)
  state, steps, states = parse_input(file_path)
  tape = {}
  cursor = 0
  checksum = 0
  
  steps.times do
    value = tape[cursor] || 0
    new_value, move, next_state = states[state][value]
    
    tape[cursor] = new_value
    cursor += move
    state = next_state
  end
  
  checksum = tape.values.sum
end

result = run_turing_machine("input.txt")
puts result
