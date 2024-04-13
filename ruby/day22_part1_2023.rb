class Coord
  attr_accessor :x, :y, :z

  def initialize(x = 0, y = 0, z = 0)
    @x, @y, @z = x, y, z
  end
end

class Brick
  attr_accessor :mini, :maxi, :based_on, :support

  def initialize
    @mini = Coord.new
    @maxi = Coord.new
    @based_on = []
    @support = []
  end
end

def parse_input(input)
  input.map do |line|
    brick = Brick.new
    coords = line.scan(/\d+/).map(&:to_i)
    brick.mini.x, brick.mini.y, brick.mini.z, brick.maxi.x, brick.maxi.y, brick.maxi.z = coords
    brick
  end
end

def settle(bricks)
  bricks.sort_by! { |brick| brick.maxi.z }
  bricks.each_with_index do |brick, i|
    support_z = 0
    based_bricks = []

    (0...i).reverse_each do |j|
      is_intersecting_x = [brick.mini.x, bricks[j].mini.x].max <= [brick.maxi.x, bricks[j].maxi.x].min
      is_intersecting_y = [brick.mini.y, bricks[j].mini.y].max <= [brick.maxi.y, bricks[j].maxi.y].min
      is_intersecting = is_intersecting_x && is_intersecting_y

      if is_intersecting
        if bricks[j].maxi.z == support_z
          based_bricks << bricks[j]
        elsif bricks[j].maxi.z > support_z
          support_z = bricks[j].maxi.z
          based_bricks = [bricks[j]]
        end
      end
    end

    brick.based_on = based_bricks
    based_bricks.each { |based_brick| based_brick.support << brick }

    delta_z = brick.maxi.z - brick.mini.z
    brick.mini.z = support_z + 1
    brick.maxi.z = brick.mini.z + delta_z
  end
end

def solve(input)
  bricks = parse_input(input)
  settle(bricks)
  bricks.count { |brick| brick.support.all? { |supported_brick| supported_brick.based_on.size >= 2 } }
end

def read_file(file_name)
  File.read(file_name).split("\n")
end

input = read_file("input.txt")
puts solve(input)