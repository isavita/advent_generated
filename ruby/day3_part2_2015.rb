houses = Hash.new(0)
santa_x, santa_y = 0, 0
robo_x, robo_y = 0, 0
santa_turn = true

houses["0,0"] += 2

File.open("input.txt").each_char do |char|
  if santa_turn
    case char
    when "^"
      santa_y += 1
    when "v"
      santa_y -= 1
    when ">"
      santa_x += 1
    when "<"
      santa_x -= 1
    end
    houses["#{santa_x},#{santa_y}"] += 1
  else
    case char
    when "^"
      robo_y += 1
    when "v"
      robo_y -= 1
    when ">"
      robo_x += 1
    when "<"
      robo_x -= 1
    end
    houses["#{robo_x},#{robo_y}"] += 1
  end
  santa_turn = !santa_turn
end

puts houses.length