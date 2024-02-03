
require 'digest'

def open_doors(passcode, path)
  hash = Digest::MD5.hexdigest(passcode + path)
  doors = hash[0..3]

  open_doors = []
  open_doors << 'U' if doors[0] =~ /[b-f]/
  open_doors << 'D' if doors[1] =~ /[b-f]/
  open_doors << 'L' if doors[2] =~ /[b-f]/
  open_doors << 'R' if doors[3] =~ /[b-f]/

  open_doors
end

def find_shortest_path(passcode)
  queue = [[0, 0, ""]]
  
  until queue.empty?
    x, y, path = queue.shift
    
    return path if x == 3 && y == 3
    
    open = open_doors(passcode, path)
    
    queue << [x, y - 1, path + 'U'] if y > 0 && open.include?('U')
    queue << [x, y + 1, path + 'D'] if y < 3 && open.include?('D')
    queue << [x - 1, y, path + 'L'] if x > 0 && open.include?('L')
    queue << [x + 1, y, path + 'R'] if x < 3 && open.include?('R')
  end
end

passcode = File.read('input.txt').strip
puts find_shortest_path(passcode)
