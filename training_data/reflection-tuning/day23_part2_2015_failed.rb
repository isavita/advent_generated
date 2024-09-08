def execute_program(instructions, initial_a = 0)
  registers = {'a' => initial_a, 'b' => 0}
  pc = 0

  while pc < instructions.length
    instruction, *args = instructions[pc].split
    
    case instruction
    when 'hlf'
      registers[args[0]] /= 2 if registers.key?(args[0])
    when 'tpl'
      registers[args[0]] *= 3 if registers.key?(args[0])
    when 'inc'
      registers[args[0]] += 1 if registers.key?(args[0])
    when 'jmp'
      pc += args[0].to_i - 1
    when 'jie'
      pc += args[1].to_i - 1 if registers.key?(args[0]) && registers[args[0]].even?
    when 'jio'
      pc += args[1].to_i - 1 if registers.key?(args[0]) && registers[args[0]] == 1
    end

    pc += 1
  end

  registers['b']
end

# Read instructions from file
instructions = File.readlines('input.txt').map(&:chomp)

# Part One
puts "Part One: #{execute_program(instructions)}"

# Part Two
puts "Part Two: #{execute_program(instructions, 1)}"
