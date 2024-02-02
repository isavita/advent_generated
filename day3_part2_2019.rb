
input = File.readlines('input.txt').map { |line| line.split(',') }

def get_points(wire)
  x = 0
  y = 0
  points = {}
  steps = 0

  wire.each do |instr|
    dir = instr[0]
    dist = instr[1..-1].to_i

    dist.times do
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

      steps += 1
      points[[x, y]] ||= steps
    end
  end

  points
end

wire1_points = get_points(input[0])
wire2_points = get_points(input[1])

intersections = wire1_points.keys & wire2_points.keys

part1 = intersections.map { |point| point[0].abs + point[1].abs }.min
part2 = intersections.map { |point| wire1_points[point] + wire2_points[point] }.min

puts part1
puts part2
