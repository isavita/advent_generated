
registers = { 'a' => 0, 'b' => 0 }
instructions = File.readlines('input.txt').map(&:strip)

i = 0
while i < instructions.length
  parts = instructions[i].split
  case parts[0]
  when 'hlf'
    registers[parts[1]] /= 2
  when 'tpl'
    registers[parts[1]] *= 3
  when 'inc'
    registers[parts[1]] += 1
  when 'jmp'
    i += parts[1].to_i
    next
  when 'jie'
    i += parts[2].to_i if registers[parts[1].chop].even?
    next if registers[parts[1].chop].even?
  when 'jio'
    i += parts[2].to_i if registers[parts[1].chop] == 1
    next if registers[parts[1].chop] == 1
  end
  i += 1
end

puts registers['b']

# Reset for part two
registers = { 'a' => 1, 'b' => 0 }
i = 0
while i < instructions.length
  parts = instructions[i].split
  case parts[0]
  when 'hlf'
    registers[parts[1]] /= 2
  when 'tpl'
    registers[parts[1]] *= 3
  when 'inc'
    registers[parts[1]] += 1
  when 'jmp'
    i += parts[1].to_i
    next
  when 'jie'
    i += parts[2].to_i if registers[parts[1].chop].even?
    next if registers[parts[1].chop].even?
  when 'jio'
    i += parts[2].to_i if registers[parts[1].chop] == 1
    next if registers[parts[1].chop] == 1
  end
  i += 1
end

puts registers['b']
