class SnailfishNumber
  attr_accessor :left, :right

  def initialize(left, right)
    @left = left.is_a?(Array) ? SnailfishNumber.new(left[0], left[1]) : left
    @right = right.is_a?(Array) ? SnailfishNumber.new(right[0], right[1]) : right
  end

  def magnitude
    3 * (left.is_a?(SnailfishNumber) ? left.magnitude : left) +
    2 * (right.is_a?(SnailfishNumber) ? right.magnitude : right)
  end

  def reduce
    loop do
      exploded, _ = explode
      next if exploded

      split = split
      break unless split
    end
    self
  end

  def explode(depth = 0)
    if depth == 4
      return true, [left, right]
    end

    if left.is_a?(SnailfishNumber)
      exploded, values = left.explode(depth + 1)
      if exploded
        if values
          self.left = 0
          add_left(right, values[1])
        end
        return true, [values[0], nil]
      end
    end

    if right.is_a?(SnailfishNumber)
      exploded, values = right.explode(depth + 1)
      if exploded
        if values
          self.right = 0
          add_right(left, values[0])
        end
        return true, [nil, values[1]]
      end
    end

    return false, nil
  end

  def split
    if left.is_a?(Integer) && left >= 10
      self.left = SnailfishNumber.new(left / 2, (left + 1) / 2)
      return true
    end

    if left.is_a?(SnailfishNumber) && left.split
      return true
    end

    if right.is_a?(Integer) && right >= 10
      self.right = SnailfishNumber.new(right / 2, (right + 1) / 2)
      return true
    end

    if right.is_a?(SnailfishNumber) && right.split
      return true
    end

    false
  end

  def add_left(node, value)
    if node.is_a?(Integer)
      node + value
    else
      node.left = add_left(node.left, value)
      node
    end
  end

  def add_right(node, value)
    if node.is_a?(Integer)
      node + value
    else
      node.right = add_right(node.right, value)
      node
    end
  end

  def +(other)
    SnailfishNumber.new(self, other).reduce
  end

  def to_s
    "[#{left},#{right}]"
  end
end

def parse(input)
  eval(input.gsub(/\[/, 'SnailfishNumber.new(').gsub(/\]/, ')'))
end

numbers = File.readlines('input.txt').map(&:strip).map { |line| parse(line) }
result = numbers.reduce(:+)
puts "Magnitude of the final sum: #{result.magnitude}"
