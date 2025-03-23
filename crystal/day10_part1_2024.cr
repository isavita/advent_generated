
def solve(map : Array(String)) : Int32
  height = map.size
  width = map[0].size
  
  trailheads = [] of Tuple(Int32, Int32)
  nines = [] of Tuple(Int32, Int32)
  
  map.each_with_index do |row, r|
    row.each_char.each_with_index do |c, col|
      if c == '0'
        trailheads << {r, col}
      elsif c == '9'
        nines << {r, col}
      end
    end
  end
  
  total_score = 0
  
  trailheads.each do |start_row, start_col|
    reachable_nines = 0
    
    nines.each do |nine_row, nine_col|
      
      # Breadth-First Search
      queue = [{start_row, start_col}]
      visited = Set(Tuple(Int32,Int32)).new
      visited.add({start_row, start_col})
      
      
      while !queue.empty?
        current_row, current_col = queue.shift
        
        if current_row == nine_row && current_col == nine_col
          reachable_nines += 1
          break
        end
        
        current_height = map[current_row][current_col].to_i
        
        # Possible moves (up, down, left, right)
        moves = [
          {current_row - 1, current_col},
          {current_row + 1, current_col},
          {current_row, current_col - 1},
          {current_row, current_col + 1}
        ]
        
        moves.each do |next_row, next_col|
          # Check bounds
          next if next_row < 0 || next_row >= height || next_col < 0 || next_col >= width
          
          # Check height difference
          next_height = map[next_row][next_col].to_i
          next if next_height != current_height + 1
          
          # Check if visited
          next if visited.includes?({next_row, next_col})
          
          queue << {next_row, next_col}
          visited.add({next_row, next_col})
        end
      end
    end
    
    total_score += reachable_nines
  end
  
  total_score
end


def main
  map = [] of String
  File.each_line("input.txt") do |line|
    map << line.strip
  end

  puts solve(map)
end

main
