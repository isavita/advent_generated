
require "digest/md5"

def get_hash(input_string : String) : String
  Digest::MD5.hexdigest(input_string)
end

def get_open_doors(hash_str : String) : Array(Bool)
  doors = [] of Bool
  hash_str[0..3].each_char do |char|
    doors << "bcdef".includes?(char)
  end
  doors
end

def is_valid_move(x : Int32, y : Int32, direction : Char) : Bool
  case direction
  when 'U'
    y > 0
  when 'D'
    y < 3
  when 'L'
    x > 0
  when 'R'
    x < 3
  else
    false
  end
end

def find_shortest_path(passcode : String) : String?
  directions = ['U', 'D', 'L', 'R']
  queue = [{x: 0, y: 0, path: ""}]

  until queue.empty?
    current = queue.shift
    x = current[:x]
    y = current[:y]
    path = current[:path]

    if x == 3 && y == 3
      return path
    end

    hash_input = passcode + path
    hash_str = get_hash(hash_input)
    open_doors = get_open_doors(hash_str)

    4.times do |i|
      if open_doors[i] && is_valid_move(x, y, directions[i])
        new_x = x
        new_y = y
        case directions[i]
        when 'U'
          new_y -= 1
        when 'D'
          new_y += 1
        when 'L'
          new_x -= 1
        when 'R'
          new_x += 1
        end
        queue << {x: new_x, y: new_y, path: path + directions[i]}
      end
    end
  end
  nil
end

passcode = File.read("input.txt").strip

shortest_path = find_shortest_path(passcode)

puts shortest_path
