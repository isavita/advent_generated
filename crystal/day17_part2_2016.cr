
require "digest/md5"

struct Point
  property x : Int32
  property y : Int32
  property path : String

  def initialize(@x : Int32, @y : Int32, @path : String)
  end
end

def read_passcode(filename : String) : String
  File.read(filename).strip
end

def find_longest_path_length(passcode : String) : Int32
  longest = 0
  queue = [Point.new(0, 0, "")]
  
  while !queue.empty?
    point = queue.shift
    
    if point.x == 3 && point.y == 3
      longest = [longest, point.path.size].max
      next
    end
    
    get_open_doors(passcode, point.path).each do |dir|
      next_point = Point.new(point.x, point.y, point.path + dir)
      case dir
      when "U"
        next_point.y -= 1
      when "D"
        next_point.y += 1
      when "L"
        next_point.x -= 1
      when "R"
        next_point.x += 1
      end
      
      if next_point.x >= 0 && next_point.x < 4 && next_point.y >= 0 && next_point.y < 4
        queue << next_point
      end
    end
  end
  longest
end

def get_open_doors(passcode : String, path : String) : Array(String)
  hash = md5_hash(passcode + path)
  doors = [] of String
  if hash[0] >= 'b' && hash[0] <= 'f'
    doors << "U"
  end
  if hash[1] >= 'b' && hash[1] <= 'f'
    doors << "D"
  end
  if hash[2] >= 'b' && hash[2] <= 'f'
    doors << "L"
  end
  if hash[3] >= 'b' && hash[3] <= 'f'
    doors << "R"
  end
  doors
end

def md5_hash(input : String) : String
  Digest::MD5.hexdigest(input)
end

passcode = read_passcode("input.txt")
longest_path_length = find_longest_path_length(passcode)
puts longest_path_length
