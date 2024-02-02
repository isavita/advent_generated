
input = File.readlines('input.txt').map { |line| line.split(',') }

def get_points(wire)
  x = 0
  y = 0
  points = []
  wire.each do |instr|
    dir = instr[0]
    steps = instr[1..].to_i
    steps.times do
      case dir
      when 'U'
        y += 1
      when 'D'
        y -= 1
      when 'L'
        x -= 1
      when 'R'
        x += 1
      end
      points << [x, y]
    end
  end
  points
end

wire1_points = get_points(input[0])
wire2_points = get_points(input[1])

intersections = (wire1_points & wire2_points) - [[0, 0]]

puts intersections.map { |point| point[0].abs + point[1].abs }.min
