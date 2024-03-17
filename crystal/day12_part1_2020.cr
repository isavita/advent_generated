struct Ship
  getter x, y, facing

  def initialize(@x : Int32, @y : Int32, @facing : Int32)
  end

  def process_instruction(action : Char, value : Int32)
    case action
    when 'N'
      @y += value
    when 'S'
      @y -= value
    when 'E'
      @x += value
    when 'W'
      @x -= value
    when 'L'
      @facing = (@facing - value + 360) % 360
    when 'R'
      @facing = (@facing + value) % 360
    when 'F'
      case @facing
      when 0
        @x += value
      when 90
        @y -= value
      when 180
        @x -= value
      when 270
        @y += value
      end
    end
  end
end

file = File.open("input.txt")
ship = Ship.new(0, 0, 0)

file.each_line do |line|
  action = line[0]
  value = line[1..-1].to_i
  ship.process_instruction(action, value)
end

manhattan_distance = ship.x.abs + ship.y.abs
puts manhattan_distance