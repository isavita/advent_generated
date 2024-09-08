require_relative 'intcode'

def parse_scaffolding(output)
  output.split("\n").map(&:chars)
end

def find_intersections(scaffolding)
  intersections = []
  (1...scaffolding.size - 1).each do |row|
    (1...scaffolding[row].size - 1).each do |col|
      if scaffolding[row][col] == '#' &&
         scaffolding[row-1][col] == '#' &&
         scaffolding[row+1][col] == '#' &&
         scaffolding[row][col-1] == '#' &&
         scaffolding[row][col+1] == '#'
        intersections << [row, col]
      end
    end
  end
  intersections
end

def calculate_alignment_parameters(intersections)
  intersections.map { |row, col| row * col }
end

input = File.read('input.txt').strip.split(',').map(&:to_i)
computer = Intcode.new(input)
output = computer.run.map(&:chr).join

scaffolding = parse_scaffolding(output)
intersections = find_intersections(scaffolding)
alignment_parameters = calculate_alignment_parameters(intersections)

sum_of_alignment_parameters = alignment_parameters.sum

puts sum_of_alignment_parameters
