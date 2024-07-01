Point = Struct.new(:x, :y)
Movement = Struct.new(:steps, :rotate)

DIRS = [
  Point.new(-1, 0),  # N
  Point.new(0, 1),   # E
  Point.new(1, 0),   # S
  Point.new(0, -1)   # W
]

class Human
  attr_accessor :curr, :facing

  def initialize(curr, facing)
    @curr = curr
    @facing = facing
  end

  def walk(map)
    dir = DIRS[@facing]
    next_point = Point.new(@curr.x + dir.x, @curr.y + dir.y)

    if map.key?(next_point)
      return false if map[next_point]
      @curr = next_point
      return true
    end

    opp_dir = Point.new(-dir.x, -dir.y)
    loop do
      look_ahead = Point.new(next_point.x + opp_dir.x, next_point.y + opp_dir.y)
      unless map.key?(look_ahead)
        return false if map[next_point]
        @curr = next_point
        return true
      end
      next_point = look_ahead
    end
  end
end

def rotate(dir, direction)
  case direction
  when 'R' then (dir + 1) % 4
  when 'L' then (dir - 1 + 4) % 4
  else dir
  end
end

def parse_path(path)
  movements = []
  acc = 0
  path.each_char do |char|
    case char
    when 'R', 'L'
      movements << Movement.new(acc, nil) if acc > 0
      movements << Movement.new(nil, char)
      acc = 0
    else
      acc = 10 * acc + char.to_i
    end
  end
  movements << Movement.new(acc, nil) if acc > 0
  movements
end

def parse_input
  map = {}
  size = 0
  movements = []

  File.readlines('input.txt', chomp: true).each_with_index do |line, r|
    if line.empty?
      movements = parse_path(File.readlines('input.txt', chomp: true).last)
      break
    end

    size = line.length / 3 if r == 0

    line.chars.each_with_index do |char, c|
      case char
      when '#' then map[Point.new(r, c)] = true
      when '.' then map[Point.new(r, c)] = false
      end
    end
  end

  [map, size, movements]
end

map, size, movements = parse_input

human = Human.new(Point.new(0, size), 1)  # 1 is East

movements.each do |mov|
  if mov.rotate
    human.facing = rotate(human.facing, mov.rotate)
  else
    mov.steps.times do
      break unless human.walk(map)
    end
  end
end

password = 1000 * (human.curr.x + 1) + 4 * (human.curr.y + 1) + ((human.facing + 3) % 4)
puts "The final password is: #{password}"
