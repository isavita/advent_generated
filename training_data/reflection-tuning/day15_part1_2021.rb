require 'set'

def dijkstra(cave)
  height, width = cave.size, cave[0].size
  goal = [height - 1, width - 1]
  
  risks = Array.new(height) { Array.new(width, Float::INFINITY) }
  risks[0][0] = 0
  
  queue = [[0, 0, 0]]  # [total_risk, row, col]
  visited = Set.new

  while !queue.empty?
    risk, row, col = queue.shift
    next if visited.include?([row, col])
    visited.add([row, col])

    return risk if [row, col] == goal

    [[0, 1], [1, 0], [0, -1], [-1, 0]].each do |dr, dc|
      new_row, new_col = row + dr, col + dc
      if new_row.between?(0, height-1) && new_col.between?(0, width-1)
        new_risk = risk + cave[new_row][new_col]
        if new_risk < risks[new_row][new_col]
          risks[new_row][new_col] = new_risk
          queue << [new_risk, new_row, new_col]
        end
      end
    end
    queue.sort!  # Keep queue sorted by risk
  end
end

cave = File.readlines('input.txt', chomp: true).map { |line| line.chars.map(&:to_i) }
puts dijkstra(cave)
