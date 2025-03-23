
struct Instruction
  property direction : Char
  property distance : Int64

  def initialize(direction : Char, distance : Int64)
    @direction = direction
    @distance = distance
  end
end

def solve(input : String, part_two : Bool) : Int64
  instructions = parse_instructions(input, part_two)
  vertices = calculate_vertices(instructions)
  area(vertices)
end

def parse_instructions(input : String, part_two : Bool) : Array(Instruction)
  instructions = [] of Instruction
  input.each_line do |line|
    parts = line.split
    if part_two
      hex_code = parts[2][2..-2]
      distance = hex_code[0..4].to_i64(16)
      direction_code = hex_code[-1]
      direction = case direction_code
                  when '0' then 'R'
                  when '1' then 'D'
                  when '2' then 'L'
                  when '3' then 'U'
                  else raise "Invalid direction code: #{direction_code}"
                  end
      instructions << Instruction.new(direction, distance)
    else
      direction = parts[0][0]
      distance = parts[1].to_i64
      instructions << Instruction.new(direction, distance)
    end
  end
  instructions
end

def calculate_vertices(instructions : Array(Instruction)) : Array(Tuple(Int64, Int64))
  vertices = [] of Tuple(Int64, Int64)
  current_x = 0_i64
  current_y = 0_i64
  vertices << {current_x, current_y}

  instructions.each do |instruction|
    case instruction.direction
    when 'R'
      current_x += instruction.distance
    when 'L'
      current_x -= instruction.distance
    when 'U'
      current_y -= instruction.distance
    when 'D'
      current_y += instruction.distance
    end
    vertices << {current_x, current_y}
  end

  vertices
end

# Shoelace formula with Pick's theorem
def area(vertices : Array(Tuple(Int64, Int64))) : Int64
  n = vertices.size - 1
  boundary = 0_i64
  area = 0_i64

  (0...n).each do |i|
    j = (i + 1) % n
    area += (vertices[i][0] * vertices[j][1]) - (vertices[j][0] * vertices[i][1])

    dx = (vertices[j][0] - vertices[i][0]).abs
    dy = (vertices[j][1] - vertices[i][1]).abs
    boundary += dx + dy
  end

  area = area.abs // 2
  area + (boundary // 2) + 1
end

def main
  input = File.read("input.txt")
  part_one_result = solve(input, false)
  puts "Part One: #{part_one_result}"

  part_two_result = solve(input, true)
  puts "Part Two: #{part_two_result}"
end

main
