
class Monkey
  property name : String
  property val : Int64
  property has_val : Bool
  property left : Monkey?
  property right : Monkey?
  property op : String

  def initialize(@name : String, @val : Int64 = 0, @has_val : Bool = false, @left : Monkey? = nil, @right : Monkey? = nil, @op : String = "")
  end

  def solve : {Int64, Bool}
    return {@val, true} if @has_val

    if left = @left
      if right = @right
        left_val, left_ok = left.solve
        right_val, right_ok = right.solve

        if left_ok && right_ok
          case @op
          when "+"
            return {left_val + right_val, true}
          when "-"
            return {left_val - right_val, true}
          when "*"
            return {left_val * right_val, true}
          when "/"
            return {left_val // right_val, true}
          when "=="
            return {left_val == right_val ? 0_i64 : 1_i64, true}
          end
        end
      end
    end

    {0_i64, false}
  end

  def expect(x : Int64) : Int64
    return x if @name == "humn"

    left_val, left_ok = @left.not_nil!.solve
    right_val, right_ok = @right.not_nil!.solve

    unless left_ok
      case @op
      when "+"
        return @left.not_nil!.expect(x - right_val)
      when "-"
        return @left.not_nil!.expect(x + right_val)
      when "*"
        return @left.not_nil!.expect(x // right_val)
      when "/"
        return @left.not_nil!.expect(x * right_val)
      when "=="
        return @left.not_nil!.expect(right_val)
      end
    end

    unless right_ok
      case @op
      when "+"
        return @right.not_nil!.expect(x - left_val)
      when "-"
        return @right.not_nil!.expect(left_val - x)
      when "*"
        return @right.not_nil!.expect(x // left_val)
      when "/"
        return @right.not_nil!.expect(left_val // x)
      when "=="
        return @right.not_nil!.expect(left_val)
      end
    end

    raise "impossible"
  end
end

def parse : Hash(String, Monkey)
  index = {} of String => Monkey

  File.read_lines("input.txt").each do |line|
    goal, rest = line.split(": ")
    index[goal] ||= Monkey.new(goal)

    if num = rest.to_i64?
      index[goal].val = num
      index[goal].has_val = true
      next
    end

    left, op, right = rest.split(" ")
    index[left] ||= Monkey.new(left)
    index[right] ||= Monkey.new(right)

    index[goal].left = index[left]
    index[goal].op = op
    index[goal].right = index[right]
  end

  index
end

index = parse
index["humn"].has_val = false
index["root"].op = "=="

puts index["root"].expect(0_i64)
