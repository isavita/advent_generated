
directions = File.read('input.txt').strip
visited = {[0, 0] => true}
x = y = 0

directions.each_char do |dir|
  case dir
  when '^' then y += 1
  when 'v' then y -= 1
  when '>' then x += 1
  when '<' then x -= 1
  end
  visited[[x, y]] = true
end

puts visited.size
