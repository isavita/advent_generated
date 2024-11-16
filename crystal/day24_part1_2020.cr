
class HexTile
  getter x, y, z

  def initialize(@x : Int32, @y : Int32, @z : Int32)
  end

  def self.from_directions(directions : String)
    x, y, z = 0, 0, 0
    i = 0
    while i < directions.size
      case directions[i]
      when 'e'
        x += 1
        z -= 1
      when 'w'
        x -= 1
        z += 1
      when 'n'
        if directions[i + 1] == 'e'
          y += 1
          z -= 1
          i += 1
        elsif directions[i + 1] == 'w'
          x -= 1
          y += 1
          i += 1
        end
      when 's'
        if directions[i + 1] == 'e'
          x += 1
          y -= 1
          i += 1
        elsif directions[i + 1] == 'w'
          y -= 1
          z += 1
          i += 1
        end
      end
      i += 1
    end
    new(x, y, z)
  end

  def ==(other : HexTile)
    x == other.x && y == other.y && z == other.z
  end

  def hash
    {x, y, z}.hash
  end
end

def solve_lobby_layout(input : Array(String)) : Int32
  black_tiles = Set(HexTile).new

  input.each do |directions|
    tile = HexTile.from_directions(directions)
    if black_tiles.includes?(tile)
      black_tiles.delete(tile)
    else
      black_tiles.add(tile)
    end
  end

  black_tiles.size
end

# Read input from file
input = File.read_lines("input.txt")

# Solve and print result
puts solve_lobby_layout(input)
