houses = Hash.new(0)
x = 0
y = 0

houses["0,0"] += 1

File.open("input.txt", "r").each_char do |char|
  case char
  when "^"
    y += 1
  when "v"
    y -= 1
  when ">"
    x += 1
  when "<"
    x -= 1
  end
  houses["#{x},#{y}"] += 1
end

puts houses.length