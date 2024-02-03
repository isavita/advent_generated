
def run_program(memory, inputs)
  pc = 0
  output = 0
  input_counter = 0
  while memory[pc] != 99
    opcode = memory[pc] % 100
    modes = (memory[pc] / 100).digits
    modes << 0 while modes.size < 3
    val1 = modes[0] == 0 ? memory[memory[pc + 1]] : memory[pc + 1]
    val2 = modes[1] == 0 ? memory[memory[pc + 2]] : memory[pc + 2] if [1, 2, 5, 6, 7, 8].include?(opcode)
    case opcode
    when 1
      memory[memory[pc + 3]] = val1 + val2
      pc += 4
    when 2
      memory[memory[pc + 3]] = val1 * val2
      pc += 4
    when 3
      memory[memory[pc + 1]] = inputs[input_counter]
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
      memory[memory[pc + 3]] = val1 < val2 ? 1 : 0
      pc += 4
    when 8
      memory[memory[pc + 3]] = val1 == val2 ? 1 : 0
      pc += 4
    end
  end
  output
end

def max_signal(program)
  phase_settings = (0..4).to_a.permutation.to_a
  max_output = 0
  phase_settings.each do |settings|
    input_signal = 0
    settings.each do |setting|
      input_signal = run_program(program.dup, [setting, input_signal])
    end
    max_output = [max_output, input_signal].max
  end
  max_output
end

program = File.read("input.txt").split(",").map(&:to_i)
puts max_signal(program)
