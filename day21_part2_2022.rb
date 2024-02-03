
class Monkey
  attr_accessor :name, :val, :has_val, :left, :right, :op

  def initialize(name)
    @name = name
    @val = 0
    @has_val = false
    @left = nil
    @right = nil
    @op = ""
  end

  def solve
    if @has_val
      return @val, true
    end

    if @left && @right
      left, l_ok = @left.solve
      right, r_ok = @right.solve

      if l_ok && r_ok
        case @op
        when "+"
          return left + right, true
        when "-"
          return left - right, true
        when "*"
          return left * right, true
        when "/"
          return left / right, true
        when "=="
          if left == right
            return 0, true
          else
            return 1, true
          end
        end
      end
    end
    return 0, false
  end

  def expect(x)
    if @name == "humn"
      return x
    end

    left, l_ok = @left.solve
    right, r_ok = @right.solve

    if !l_ok
      case @op
      when "+"
        return @left.expect(x - right)
      when "-"
        return @left.expect(x + right)
      when "*"
        return @left.expect(x / right)
      when "/"
        return @left.expect(x * right)
      when "=="
        return @left.expect(right)
      end
    end

    if !r_ok
      case @op
      when "+"
        return @right.expect(x - left)
      when "-"
        return @right.expect(left - x)
      when "*"
        return @right.expect(x / left)
      when "/"
        return @right.expect(left / x)
      when "=="
        return @right.expect(left)
      end
    end

    raise "impossible"
  end
end

def parse
  index = {}

  File.open("input.txt", "r") do |file|
    file.each_line do |line|
      ff = line.chomp.split(": ")
      goal = ff[0]

      index[goal] ||= Monkey.new(goal)

      if ff[1].to_i.to_s == ff[1]
        index[goal].val = ff[1].to_i
        index[goal].has_val = true
        next
      end

      r = ff[1].split(" ")
      left, op, right = r[0], r[1], r[2]

      index[left] ||= Monkey.new(left)
      index[right] ||= Monkey.new(right)

      index[goal].left = index[left]
      index[goal].op = op
      index[goal].right = index[right]
    end
  end

  index
end

index = parse

index["humn"].has_val = false
index["root"].op = "=="

puts index["root"].expect(0)
