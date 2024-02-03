
input = File.read('input.txt').strip.split(',').map(&:to_i)

input[1] = 12
input[2] = 2

i = 0
while input[i] != 99
  opcode = input[i]
  num1 = input[input[i + 1]]
  num2 = input[input[i + 2]]
  output_pos = input[i + 3]

  if opcode == 1
    input[output_pos] = num1 + num2
  elsif opcode == 2
    input[output_pos] = num1 * num2
  end

  i += 4
end

puts input[0]
