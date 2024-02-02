
input = File.readlines('input.txt').map(&:chomp)

registers = { 'a' => 0, 'b' => 0 }
current_instruction = 0

while current_instruction < input.length
  instruction = input[current_instruction].split

  case instruction[0]
  when 'hlf'
    registers[instruction[1]] /= 2
    current_instruction += 1
  when 'tpl'
    registers[instruction[1]] *= 3
    current_instruction += 1
  when 'inc'
    registers[instruction[1]] += 1
    current_instruction += 1
  when 'jmp'
    current_instruction += instruction[1].to_i
  when 'jie'
    if registers[instruction[1].delete(',')] % 2 == 0
      current_instruction += instruction[2].to_i
    else
      current_instruction += 1
    end
  when 'jio'
    if registers[instruction[1].delete(',')] == 1
      current_instruction += instruction[2].to_i
    else
      current_instruction += 1
    end
  end
end

puts registers['b']
