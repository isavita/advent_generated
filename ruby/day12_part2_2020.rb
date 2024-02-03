
def rotate_waypoint(waypoint, direction, degrees)
  steps = degrees / 90
  steps.times do
    waypoint = direction == 'R' ? [waypoint[1], -waypoint[0]] : [-waypoint[1], waypoint[0]]
  end
  waypoint
end

def part_one(instructions)
  direction = 90
  position = [0, 0] # [east/west, north/south]
  instructions.each do |instruction|
    action, value = instruction[0], instruction[1..].to_i
    case action
    when 'N' then position[1] += value
    when 'S' then position[1] -= value
    when 'E' then position[0] += value
    when 'W' then position[0] -= value
    when 'L' then direction = (direction - value) % 360
    when 'R' then direction = (direction + value) % 360
    when 'F'
      case direction
      when 0 then position[1] += value
      when 180 then position[1] -= value
      when 90 then position[0] += value
      when 270 then position[0] -= value
      end
    end
  end
  position[0].abs + position[1].abs
end

def part_two(instructions)
  ship = [0, 0]
  waypoint = [10, 1] # [east/west, north/south] relative to the ship
  instructions.each do |instruction|
    action, value = instruction[0], instruction[1..].to_i
    case action
    when 'N' then waypoint[1] += value
    when 'S' then waypoint[1] -= value
    when 'E' then waypoint[0] += value
    when 'W' then waypoint[0] -= value
    when 'L' then waypoint = rotate_waypoint(waypoint, 'L', value)
    when 'R' then waypoint = rotate_waypoint(waypoint, 'R', value)
    when 'F'
      ship[0] += waypoint[0] * value
      ship[1] += waypoint[1] * value
    end
  end
  ship[0].abs + ship[1].abs
end

instructions = File.readlines('input.txt', chomp: true)
puts part_one(instructions)
puts part_two(instructions)
