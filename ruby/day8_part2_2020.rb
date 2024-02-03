
input = File.readlines('input.txt').map(&:chomp)

def run_program(instructions)
  acc = 0
  index = 0
  visited = []

  while index < instructions.length
    break if visited.include?(index)

    visited << index
    operation, argument = instructions[index].split

    case operation
    when 'acc'
      acc += argument.to_i
      index += 1
    when 'jmp'
      index += argument.to_i
    when 'nop'
      index += 1
    end
  end

  [index >= instructions.length, acc]
end

# Part One
puts run_program(input)[1]

# Part Two
input.each_with_index do |instruction, i|
  modified_input = input.dup
  operation, argument = instruction.split

  if operation == 'jmp'
    modified_input[i] = "nop #{argument}"
  elsif operation == 'nop'
    modified_input[i] = "jmp #{argument}"
  end

  terminated, acc = run_program(modified_input)
  if terminated
    puts acc
    break
  end
end
