
struct Coord
  property x : Int32
  property y : Int32
  property z : Int32

  def initialize(@x : Int32, @y : Int32, @z : Int32)
  end
end

class Brick
  property mini : Coord
  property maxi : Coord
  property based_on : Array(Brick)
  property support : Array(Brick)

  def initialize(@mini : Coord, @maxi : Coord)
    @based_on = [] of Brick
    @support = [] of Brick
  end
end

def parse_input(input : Array(String)) : Array(Brick)
  input.map do |line|
    coords = line.split('~').flat_map { |part| part.split(',').map(&.to_i) }
    Brick.new(
      Coord.new(coords[0], coords[1], coords[2]),
      Coord.new(coords[3], coords[4], coords[5])
    )
  end
end

def settle(bricks : Array(Brick))
  bricks.sort_by! { |brick| brick.maxi.z }

  bricks.each_with_index do |brick, i|
    support_z = 0
    based_bricks = [] of Brick

    (0...i).reverse_each do |j|
      other_brick = bricks[j]
      is_intersecting_x = {brick.mini.x, other_brick.mini.x}.max <= {brick.maxi.x, other_brick.maxi.x}.min
      is_intersecting_y = {brick.mini.y, other_brick.mini.y}.max <= {brick.maxi.y, other_brick.maxi.y}.min
      is_intersecting = is_intersecting_x && is_intersecting_y

      if is_intersecting
        if other_brick.maxi.z == support_z
          based_bricks << other_brick
        elsif other_brick.maxi.z > support_z
          support_z = other_brick.maxi.z
          based_bricks = [other_brick]
        end
      end
    end

    brick.based_on = based_bricks
    based_bricks.each do |based_brick|
      based_brick.support << brick
    end

    delta_z = brick.maxi.z - brick.mini.z
    brick.mini.z = support_z + 1
    brick.maxi.z = brick.mini.z + delta_z
  end
end

def solve(input : Array(String)) : Int32
  bricks = parse_input(input)
  settle(bricks)

  bricks.count do |brick|
    brick.support.all? { |supported_brick| supported_brick.based_on.size >= 2 }
  end
end

input = File.read_lines("input.txt")
puts solve(input)
