
def run_program(memory, inputs)
  pc = 0
  output = 0
  input_counter = 0
  while memory[pc] != 99
    opcode = memory[pc] % 100
    modes = memory[pc].digits.drop(2)
    modes << 0 while modes.size < 3
    val1 = modes[0] == 1 ? memory[pc+1] : memory[memory[pc+1]]
    val2 = modes[1] == 1 ? memory[pc+2] : memory[memory[pc+2]] if [1,2,5,6,7,8].include?(opcode)
    case opcode
    when 1
      memory[memory[pc+3]] = val1 + val2
      pc += 4
    when 2
      memory[memory[pc+3]] = val1 * val2
      pc += 4
    when 3
      memory[memory[pc+1]] = inputs[input_counter]
      input_counter += 1
      pc += 2
    when 4
      output = val1
      pc += 2
    when 5
      pc = val1 != 0 ? val2 : pc + 3
    when 6
      pc = val1 == 0 ? val2 : pc + 3
    when 7
      memory[memory[pc+3]] = val1 < val2 ? 1 : 0
      pc += 4
    when 8
      memory[memory[pc+3]] = val1 == val2 ? 1 : 0
      pc += 4
    end
  end
  output
end

def amplify_signal(program, phases)
  signal = 0
  phases.each do |phase|
    signal = run_program(program.dup, [phase, signal])
  end
  signal
end

def feedback_loop(program, phases)
  amplifiers = Array.new(5) { {pc: 0, memory: program.dup, inputs: [], halted: false} }
  phases.each_with_index { |phase, i| amplifiers[i][:inputs] << phase }
  amplifiers.first[:inputs] << 0
  output = 0
  until amplifiers.last[:halted]
    amplifiers.each_with_index do |amp, i|
      next_amp = amplifiers[(i + 1) % 5]
      while amp[:memory][amp[:pc]] != 99 && amp[:memory][amp[:pc]] % 100 != 4
        opcode = amp[:memory][amp[:pc]] % 100
        modes = amp[:memory][amp[:pc]].digits.drop(2)
        modes << 0 while modes.size < 3
        val1 = modes[0] == 1 ? amp[:memory][amp[:pc]+1] : amp[:memory][amp[:memory][amp[:pc]+1]]
        val2 = modes[1] == 1 ? amp[:memory][amp[:pc]+2] : amp[:memory][amp[:memory][amp[:pc]+2]] if [1,2,5,6,7,8].include?(opcode)
        case opcode
        when 1
          amp[:memory][amp[:memory][amp[:pc]+3]] = val1 + val2
          amp[:pc] += 4
        when 2
          amp[:memory][amp[:memory][amp[:pc]+3]] = val1 * val2
          amp[:pc] += 4
        when 3
          amp[:memory][amp[:memory][amp[:pc]+1]] = amp[:inputs].shift
          amp[:pc] += 2
        when 5
          amp[:pc] = val1 != 0 ? val2 : amp[:pc] + 3
        when 6
          amp[:pc] = val1 == 0 ? val2 : amp[:pc] + 3
        when 7
          amp[:memory][amp[:memory][amp[:pc]+3]] = val1 < val2 ? 1 : 0
          amp[:pc] += 4
        when 8
          amp[:memory][amp[:memory][amp[:pc]+3]] = val1 == val2 ? 1 : 0
          amp[:pc] += 4
        end
      end
      if amp[:memory][amp[:pc]] == 99
        amp[:halted] = true
      elsif amp[:memory][amp[:pc]] % 100 == 4
        modes = amp[:memory][amp[:pc]].digits.drop(2)
        modes << 0 while modes.size < 3
        val1 = modes[0] == 1 ? amp[:memory][amp[:pc]+1] : amp[:memory][amp[:memory][amp[:pc]+1]]
        output = val1
        next_amp[:inputs] << output
        amp[:pc] += 2
      end
    end
  end
  output
end

program = File.read("input.txt").split(",").map(&:to_i)
max_signal = (0..4).to_a.permutation.map { |phases| amplify_signal(program, phases) }.max
max_feedback_loop_signal = (5..9).to_a.permutation.map { |phases| feedback_loop(program, phases) }.max
puts max_signal
puts max_feedback_loop_signal
