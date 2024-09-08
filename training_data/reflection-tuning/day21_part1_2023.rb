require 'set'

def count_reachable_plots(map, steps)
  start = find_start(map)
  height, width = map.size, map[0].size
  
  even_positions = Set.new([start])
  odd_positions = Set.new

  steps.times do |step|
    current_set = step.even? ? even_positions : odd_positions
    next_set = step.even? ? odd_positions : even_positions
    
    next_set.clear
    current_set.each do |x, y|
      [[0, 1], [0, -1], [1, 0], [-1, 0]].each do |dx, dy|
        nx, ny = x + dx, y + dy
        if nx.between?(0, height - 1) && ny.between?(0, width - 1) && map[nx][ny] != '#'
          next_set.add([nx, ny])
        end
      end
    end
  end

  steps.even? ? even_positions.size : odd_positions.size
end

def find_start(map)
  map.each_with_index do |row, x|
    y = row.index('S')
    return [x, y] if y
  end
end

map = File.readlines('input.txt', chomp: true)
puts count_reachable_plots(map, 64)
