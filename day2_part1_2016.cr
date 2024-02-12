
instructions = File.read("input.txt").lines

keypad = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
x = 1
y = 1
code = ""

instructions.each do |instruction|
  instruction.each_char do |move|
    case move
    when 'U'
      x -= 1 if x > 0
    when 'D'
      x += 1 if x < 2
    when 'L'
      y -= 1 if y > 0
    when 'R'
      y += 1 if y < 2
    end
  end
  code += keypad[x][y].to_s
end

puts code
