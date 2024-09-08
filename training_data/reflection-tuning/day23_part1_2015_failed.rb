def execute_program(instructions)
  registers = {'a' => 0, 'b' => 0}
  index = 0

  while index < instructions.length
    instruction, *args = instructions[index].split

    case instruction
    when 'hlf'
      registers[args[0]] /= 2
      index += 1
    when 'tpl'
      registers[args[0]] *= 3
      index += 1
    when 'inc'
      registers[args[0]] += 1
      index += 1
    when 'jmp'
      index += args[0].to_i
    when 'jie'
      if registers[args[0]]&.even?
        index += args[1].to_i
      else
        index += 1
      end
    when 'jio'
      if registers[args[0]] == 1
        index += args[1].to_i
      else
        index += 1
      end
    else
      raise "Unknown instruction: #{instruction}"
    end
  end

  registers['b']
end

# Read instructions from file
instructions = File.readlines('input.txt').map(&:chomp)

# Execute the program and print the result
puts execute_program(instructions)
