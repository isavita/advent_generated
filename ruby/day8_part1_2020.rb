accumulator = 0
index = 0
instructions = File.readlines('input.txt').map(&:chomp)
executed = Array.new(instructions.length, false)

while index < instructions.length
  break if executed[index]

  executed[index] = true
  operation, argument = instructions[index].split(' ')

  case operation
  when 'acc'
    accumulator += argument.to_i
    index += 1
  when 'jmp'
    index += argument.to_i
  when 'nop'
    index += 1
  end
end

puts accumulator