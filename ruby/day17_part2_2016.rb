
require 'digest'

PASSCODE = File.read('input.txt').strip

def open_doors(hash)
  doors = []
  hash.chars.each_with_index do |char, idx|
    doors << ['U', 'D', 'L', 'R'][idx] if char.match(/[b-f]/)
  end
  doors
end

def valid_moves(path, x, y)
  hash = Digest::MD5.hexdigest(PASSCODE + path)
  doors = open_doors(hash)
  
  valid_moves = []
  valid_moves << 'U' if doors.include?('U') && y > 0
  valid_moves << 'D' if doors.include?('D') && y < 3
  valid_moves << 'L' if doors.include?('L') && x > 0
  valid_moves << 'R' if doors.include?('R') && x < 3
  
  valid_moves
end

def find_shortest_path
  queue = [[0, 0, ""]]
  
  until queue.empty?
    x, y, path = queue.shift
    
    return path if x == 3 && y == 3
    
    valid_moves(path, x, y).each do |move|
      new_x = x + (move == 'R' ? 1 : (move == 'L' ? -1 : 0))
      new_y = y + (move == 'D' ? 1 : (move == 'U' ? -1 : 0))
      new_path = path + move
      
      queue << [new_x, new_y, new_path]
    end
  end
end

def find_longest_path
  queue = [[0, 0, ""]]
  longest_path = ""
  
  until queue.empty?
    x, y, path = queue.shift
    
    if x == 3 && y == 3
      longest_path = path if path.length > longest_path.length
      next
    end
    
    valid_moves(path, x, y).each do |move|
      new_x = x + (move == 'R' ? 1 : (move == 'L' ? -1 : 0))
      new_y = y + (move == 'D' ? 1 : (move == 'U' ? -1 : 0))
      new_path = path + move
      
      queue << [new_x, new_y, new_path]
    end
  end
  
  longest_path.length
end

puts find_shortest_path
puts find_longest_path
