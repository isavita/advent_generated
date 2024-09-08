def longest_hike(map)
  start = [0, map[0].index('.')]
  goal = [map.size - 1, map[-1].index('.')]
  
  def dfs(map, pos, visited, length, goal)
    return length if pos == goal
    
    max_length = 0
    directions = [[0, 1], [1, 0], [0, -1], [-1, 0]]
    slopes = {'>' => [0, 1], 'v' => [1, 0], '<' => [0, -1], '^' => [-1, 0]}
    
    directions.each do |dy, dx|
      ny, nx = pos[0] + dy, pos[1] + dx
      
      next if ny < 0 || ny >= map.size || nx < 0 || nx >= map[0].size
      next if visited.include?([ny, nx])
      next if map[ny][nx] == '#'
      
      if slopes.key?(map[ny][nx])
        sy, sx = slopes[map[ny][nx]]
        ny, nx = ny + sy, nx + sx
        next if ny < 0 || ny >= map.size || nx < 0 || nx >= map[0].size
        next if visited.include?([ny, nx])
        next if map[ny][nx] == '#'
      end
      
      new_visited = visited + [[ny, nx]]
      path_length = dfs(map, [ny, nx], new_visited, length + 1, goal)
      max_length = [max_length, path_length].max
    end
    
    max_length
  end
  
  dfs(map, start, [start], 0, goal)
end

# Read input
map = File.readlines('input.txt').map(&:chomp)

# Find the longest hike
result = longest_hike(map)
puts result
