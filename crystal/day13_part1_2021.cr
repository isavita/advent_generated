class TransparentOrigami
  def initialize
    @dots = {} of Tuple(Int32, Int32) => Bool
    @instructions = [] of Tuple(Char, Int32)
  end

  def read_input(file_name : String)
    File.each_line(file_name) do |line|
      if line.includes?(',')
        x, y = line.split(',').map(&.to_i)
        @dots[{x, y}] = true
      elsif line.includes?("fold")
        axis, value = line.split('=')
        axis = axis.split.last[0]
        value = value.to_i
        @instructions << {axis, value}
      end
    end
  end

  def fold(axis : Char, value : Int32)
    new_dots = {} of Tuple(Int32, Int32) => Bool
    @dots.each do |dot, _|
      x, y = dot
      if axis == 'x' && x > value
        new_dots[{2 * value - x, y}] = true
      elsif axis == 'y' && y > value
        new_dots[{x, 2 * value - y}] = true
      else
        new_dots[dot] = true
      end
    end
    @dots = new_dots
  end

  def visible_dots
    @dots.size
  end

  def solve
    read_input("input.txt")
    axis, value = @instructions[0]
    fold(axis, value)
    puts "Visible dots after first fold: #{visible_dots}"
  end
end

TransparentOrigami.new.solve