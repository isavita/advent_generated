def main
  input = File.read('input.txt')
  computer = parse_input(input)
  until computer.tick
    break if computer.registers[computer.instruction_pointer] == 28
  end
  puts computer.registers[5]
end

class OpcodeComputer
  attr_accessor :instructions, :registers, :instruction_pointer

  def initialize(instructions, instruction_pointer)
    @instructions = instructions
    @instruction_pointer = instruction_pointer
    @registers = [0] * 6
  end

  def tick
    if @registers[@instruction_pointer] >= @instructions.size
      puts "Out of range instruction, terminating..."
      return true
    end
    inst_index = @registers[@instruction_pointer]
    inst = @instructions[inst_index]
    opcode_func = OPCODE_NAMES_TO_FUNCS[inst[:name]]
    @registers = opcode_func.call(@registers, inst[:abc_values])
    @registers[@instruction_pointer] += 1
    @registers[@instruction_pointer] >= @instructions.size
  end
end

def parse_input(input)
  lines = input.split("\n")
  instruction_pointer = lines[0].split.last.to_i
  instructions = lines[1..-1].map do |line|
    parts = line.split
    { name: parts[0], abc_values: parts[1..3].map(&:to_i) }
  end
  OpcodeComputer.new(instructions, instruction_pointer)
end

OPCODE_NAMES_TO_FUNCS = {
  'addr' => lambda { |regs, abc| regs[abc[2]] = regs[abc[0]] + regs[abc[1]]; regs },
  'addi' => lambda { |regs, abc| regs[abc[2]] = regs[abc[0]] + abc[1]; regs },
  'mulr' => lambda { |regs, abc| regs[abc[2]] = regs[abc[0]] * regs[abc[1]]; regs },
  'muli' => lambda { |regs, abc| regs[abc[2]] = regs[abc[0]] * abc[1]; regs },
  'banr' => lambda { |regs, abc| regs[abc[2]] = regs[abc[0]] & regs[abc[1]]; regs },
  'bani' => lambda { |regs, abc| regs[abc[2]] = regs[abc[0]] & abc[1]; regs },
  'borr' => lambda { |regs, abc| regs[abc[2]] = regs[abc[0]] | regs[abc[1]]; regs },
  'bori' => lambda { |regs, abc| regs[abc[2]] = regs[abc[0]] | abc[1]; regs },
  'setr' => lambda { |regs, abc| regs[abc[2]] = regs[abc[0]]; regs },
  'seti' => lambda { |regs, abc| regs[abc[2]] = abc[0]; regs },
  'gtir' => lambda { |regs, abc| regs[abc[2]] = abc[0] > regs[abc[1]] ? 1 : 0; regs },
  'gtri' => lambda { |regs, abc| regs[abc[2]] = regs[abc[0]] > abc[1] ? 1 : 0; regs },
  'gtrr' => lambda { |regs, abc| regs[abc[2]] = regs[abc[0]] > regs[abc[1]] ? 1 : 0; regs },
  'eqir' => lambda { |regs, abc| regs[abc[2]] = abc[0] == regs[abc[1]] ? 1 : 0; regs },
  'eqri' => lambda { |regs, abc| regs[abc[2]] = regs[abc[0]] == abc[1] ? 1 : 0; regs },
  'eqrr' => lambda { |regs, abc| regs[abc[2]] = regs[abc[0]] == regs[abc[1]] ? 1 : 0; regs }
}

main