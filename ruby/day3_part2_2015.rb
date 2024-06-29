
directions = File.read('input.txt').strip
visited = {[0, 0] => true}
santa = [0, 0]
robo = [0, 0]

directions.each_char.with_index do |dir, i|
  current = i.even? ? santa : robo
  case dir
  when '^' then current[1] += 1
  when 'v' then current[1] -= 1
  when '>' then current[0] += 1
  when '<' then current[0] -= 1
  end
  visited[current.dup] = true
end

puts visited.size
