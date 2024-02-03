
class Scrambler
  def initialize(pw)
    @pw = pw.chars
  end

  def to_s
    @pw.join
  end

  def swap_positions(x, y)
    @pw[x], @pw[y] = @pw[y], @pw[x]
  end

  def swap_letters(x, y)
    swap_positions(@pw.index(x), @pw.index(y))
  end

  def rotate(steps)
    steps %= @pw.length
    steps += @pw.length if steps < 0
    @pw = @pw.rotate(-steps)
  end

  def rotate_letter(x)
    index = @pw.index(x)
    index += 1 if index >= 4
    rotate(index + 1)
  end

  def derotate_letter(x)
    index = @pw.index(x)
    rot = if index.odd?
            -(index + 1) / 2
          elsif index != 0
            (6 - index) / 2
          else
            -1
          end
    rotate(rot)
  end

  def reverse(x, y)
    @pw[x..y] = @pw[x..y].reverse
  end

  def move(x, y)
    ch = @pw.delete_at(x)
    @pw.insert(y, ch)
  end

  def scramble(instructions, direction)
    instructions.reverse! if direction < 0
    instructions.each do |instruction|
      line = instruction.split
      case
      when instruction.start_with?("swap")
        x, y = line[2], line[-1]
        if line[1] == "position"
          swap_positions(x.to_i, y.to_i)
        else
          swap_letters(x, y)
        end
      when instruction.start_with?("rotate")
        if line[1] == "based"
          direction > 0 ? rotate_letter(line[-1]) : derotate_letter(line[-1])
        else
          x = line[2].to_i
          x = -x if line[1] == "left"
          x = -x if direction < 0
          rotate(x)
        end
      when instruction.start_with?("reverse")
        x, y = line[2], line[-1]
        reverse(x.to_i, y.to_i)
      when instruction.start_with?("move")
        x, y = line[2], line[-1]
        x, y = y.to_i, x.to_i if direction < 0
        move(x, y)
      end
    end
    self
  end

  def unscramble(instructions)
    scramble(instructions, -1)
  end
end

instructions = File.readlines("input.txt").map(&:chomp)
hashed = "fbgdceah"
scrambler = Scrambler.new(hashed)
result = scrambler.unscramble(instructions)
puts result
