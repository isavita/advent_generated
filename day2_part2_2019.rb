
def run_intcode_program(program, noun, verb)
  program[1] = noun
  program[2] = verb

  i = 0
  while i < program.length
    opcode = program[i]
    break if opcode == 99

    input1 = program[program[i + 1]]
    input2 = program[program[i + 2]]
    output_position = program[i + 3]

    if opcode == 1
      program[output_position] = input1 + input2
    elsif opcode == 2
      program[output_position] = input1 * input2
    end

    i += 4
  end

  program[0]
end

program = File.read('input.txt').split(',').map(&:to_i)

# Part One
puts run_intcode_program(program.dup, 12, 2)

# Part Two
(0..99).each do |noun|
  (0..99).each do |verb|
    result = run_intcode_program(program.dup, noun, verb)
    if result == 19690720
      puts 100 * noun + verb
      break
    end
  end
end
