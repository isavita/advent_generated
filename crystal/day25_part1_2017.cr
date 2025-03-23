
def read_blueprint(filename : String)
  lines = File.read_lines(filename).map(&.strip).reject(&.empty?)

  initial_state = lines[0].split.last.not_nil!.rchop('.')
  steps = lines[1].split[-2].to_i

  states = Hash(Char, Hash(Int32, Tuple(Int32, Int32, Char))).new

  i = 2
  while i < lines.size
    state_name = lines[i][-2]
    i += 1

    states[state_name] = Hash(Int32, Tuple(Int32, Int32, Char)).new
    2.times do
      val = lines[i][-2].to_i
      write_val = lines[i + 1].split.last[0].to_i
      move_dir = lines[i + 2].includes?("left") ? -1 : 1
      next_state = lines[i + 3].split.last.not_nil!.rchop('.')[0]
      states[state_name][val] = {write_val, move_dir, next_state}
      i += 4
    end
  end

  {initial_state[0], steps, states}
end

def run_turing_machine(initial_state : Char, steps : Int32, states : Hash(Char, Hash(Int32, Tuple(Int32, Int32, Char))))
  tape = Hash(Int32, Int32).new(0)
  cursor = 0
  state = initial_state

  steps.times do
    current_val = tape[cursor]
    write_val, move_dir, next_state = states[state][current_val]
    tape[cursor] = write_val
    cursor += move_dir
    state = next_state
  end

  tape.values.sum
end

initial_state, steps, states = read_blueprint("input.txt")
checksum = run_turing_machine(initial_state, steps, states)
puts checksum
