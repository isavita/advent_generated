
keypad = [
  [1, 2, 3],
  [4, 5, 6],
  [7, 8, 9]
]

current_position = [1, 1]
code = ""

File.open("input.txt").each do |line|
  line.strip.each_char do |char|
    case char
    when "U"
      current_position[0] -= 1 if current_position[0] > 0
    when "D"
      current_position[0] += 1 if current_position[0] < 2
    when "L"
      current_position[1] -= 1 if current_position[1] > 0
    when "R"
      current_position[1] += 1 if current_position[1] < 2
    end
  end
  code += keypad[current_position[0]][current_position[1]].to_s
end

puts code
