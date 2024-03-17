struct Ship
  property x : Int32
  property y : Int32
  property waypointX : Int32
  property waypointY : Int32

  def initialize(@x, @y, @waypointX, @waypointY)
  end

  def process_instruction(action : Char, value : Int32)
    case action
    when 'N'
      @waypointY += value
    when 'S'
      @waypointY -= value
    when 'E'
      @waypointX += value
    when 'W'
      @waypointX -= value
    when 'L'
      rotate_waypoint(-value)
    when 'R'
      rotate_waypoint(value)
    when 'F'
      @x += @waypointX * value
      @y += @waypointY * value
    end
  end

  def rotate_waypoint(degrees : Int32)
    degrees = (degrees + 360) % 360
    case degrees
    when 90, -270
      @waypointX, @waypointY = @waypointY, -@waypointX
    when 180, -180
      @waypointX, @waypointY = -@waypointX, -@waypointY
    when 270, -90
      @waypointX, @waypointY = -@waypointY, @waypointX
    end
  end
end

file = File.open("input.txt")
ship = Ship.new(0, 0, 10, 1)

file.each_line do |line|
  action = line[0]
  value = line[1..].to_i
  ship.process_instruction(action, value)
end

manhattan_distance = ship.x.abs + ship.y.abs
puts manhattan_distance