
class RopeBridge
  getter visited_positions : Set(Tuple(Int32, Int32))
  getter knots : Array(Tuple(Int32, Int32))

  def initialize(knot_count : Int32)
    @visited_positions = Set(Tuple(Int32, Int32)).new
    @knots = Array.new(knot_count) { {0, 0} }
    @visited_positions.add(@knots.last)
  end

  def move(direction : Char, steps : Int32)
    steps.times do
      move_head(direction)
      move_tail
      @visited_positions.add(@knots.last)
    end
  end

  private def move_head(direction : Char)
    x, y = @knots.first
    case direction
    when 'R' then @knots[0] = {x + 1, y}
    when 'L' then @knots[0] = {x - 1, y}
    when 'U' then @knots[0] = {x, y + 1}
    when 'D' then @knots[0] = {x, y - 1}
    end
  end

  private def move_tail
    (1...@knots.size).each do |i|
      head_x, head_y = @knots[i - 1]
      tail_x, tail_y = @knots[i]

      dx = head_x - tail_x
      dy = head_y - tail_y

      if dx.abs > 1 || dy.abs > 1
        tail_x += dx <=> 0
        tail_y += dy <=> 0
        @knots[i] = {tail_x, tail_y}
      end
    end
  end
end

def solve_rope_bridge(input_file : String, knot_count : Int32)
  rope = RopeBridge.new(knot_count)

  File.each_line(input_file) do |line|
    direction, steps = line.split
    rope.move(direction[0], steps.to_i)
  end

  rope.visited_positions.size
end

# Part 1
puts "Part 1: #{solve_rope_bridge("input.txt", 2)}"

# Part 2
puts "Part 2: #{solve_rope_bridge("input.txt", 10)}"
