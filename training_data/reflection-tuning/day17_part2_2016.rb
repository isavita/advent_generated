require 'digest'

PASSCODE = File.read('input.txt').strip
TARGET = [3, 3]

def md5_hash(string)
  Digest::MD5.hexdigest(string)[0...4]
end

def open_doors(hash)
  hash.chars.map { |c| c.match?(/[b-f]/) }
end

def valid_moves(x, y, doors)
  moves = []
  moves << ['U', x, y - 1] if y > 0 && doors[0]
  moves << ['D', x, y + 1] if y < 3 && doors[1]
  moves << ['L', x - 1, y] if x > 0 && doors[2]
  moves << ['R', x + 1, y] if x < 3 && doors[3]
  moves
end

def find_paths
  queue = [['', 0, 0]]
  shortest_path = nil
  longest_path_length = 0

  until queue.empty?
    path, x, y = queue.shift
    if [x, y] == TARGET
      shortest_path ||= path
      longest_path_length = [longest_path_length, path.length].max
      next
    end

    hash = md5_hash(PASSCODE + path)
    doors = open_doors(hash)
    
    valid_moves(x, y, doors).each do |move, new_x, new_y|
      queue << [path + move, new_x, new_y]
    end
  end

  [shortest_path, longest_path_length]
end

shortest_path, longest_path_length = find_paths
puts "Part 1: #{shortest_path}"
puts "Part 2: #{longest_path_length}"
