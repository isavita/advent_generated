
class Coord
  attr_accessor :x, :y, :z
end

class Brick
  attr_accessor :mini, :maxi, :basedOn, :support

  def initialize
    @mini = Coord.new
    @maxi = Coord.new
    @basedOn = []
    @support = []
  end
end

def parse_input(input)
  bricks = []
  input.each do |line|
    brick = Brick.new
    values = line.split("~").map { |val| val.split(",").map(&:to_i) }
    brick.mini.x, brick.mini.y, brick.mini.z = values[0]
    brick.maxi.x, brick.maxi.y, brick.maxi.z = values[1]
    bricks << brick
  end
  bricks
end

def settle(bricks)
  bricks.sort_by! { |brick| brick.maxi.z }

  bricks.each_with_index do |brick, i|
    support_z = 0
    based_bricks = []

    (i - 1).downto(0) do |j|
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

    brick.basedOn = based_bricks
    based_bricks.each { |based_brick| based_brick.support << brick }

    delta_z = brick.maxi.z - brick.mini.z
    brick.mini.z = support_z + 1
    brick.maxi.z = brick.mini.z + delta_z
  end
end

def solve(input)
  bricks = parse_input(input)
  settle(bricks)

  cnt = 0
  bricks.each do |brick|
    falling_bricks = {}
    brick.support.each do |supported_brick|
      if supported_brick.basedOn.length == 1
        all_supported_bricks = [supported_brick]
        until all_supported_bricks.empty?
          supported_brick0 = all_supported_bricks.shift

          is_falling = true
          supported_brick0.basedOn.each do |based_brick|
            unless falling_bricks.key?(based_brick) || based_brick == brick
              is_falling = false
              break
            end
          end

          if is_falling
            falling_bricks[supported_brick0] = true
            all_supported_bricks.concat(supported_brick0.support)
          end
        end
      end
    end
    cnt += falling_bricks.length
  end
  cnt
end

input = File.readlines("input.txt").map(&:chomp)
puts solve(input)
