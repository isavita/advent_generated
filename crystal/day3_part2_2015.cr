File.open("input.txt") do |file|
  directions = file.gets_to_end.strip
  santa_x = santa_y = robo_x = robo_y = 0
  houses = Set{Tuple.new(0, 0)}
  directions.each_char.with_index do |dir, i|
    if i.even?
      case dir
      when '^' then santa_y += 1
      when 'v' then santa_y -= 1
      when '>' then santa_x += 1
      when '<' then santa_x -= 1
      end
      houses.add(Tuple.new(santa_x, santa_y))
    else
      case dir
      when '^' then robo_y += 1
      when 'v' then robo_y -= 1
      when '>' then robo_x += 1
      when '<' then robo_x -= 1
      end
      houses.add(Tuple.new(robo_x, robo_y))
    end
  end
  puts houses.size
end