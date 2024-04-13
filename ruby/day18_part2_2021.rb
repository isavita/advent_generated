class SnailNumber
  attr_accessor :value, :left, :right

  def initialize(value: nil, left: nil, right: nil)
    @value, @left, @right = value, left, right
  end

  def regular?
    left.nil? && right.nil?
  end

  def add(other)
    SnailNumber.new(left: self.deep_copy, right: other.deep_copy).reduce
  end

  def reduce
    loop do
      exploded, _, _ = explode(0)
      break unless exploded || split
    end
    self
  end

  def explode(depth)
    return [false, 0, 0] if regular?

    if depth == 4
      left_value, right_value = left.value, right.value
      self.left = self.right = nil
      self.value = 0
      return [true, left_value, right_value]
    end

    exploded, left_value, right_value = left.explode(depth + 1)
    if exploded
      right.add_left(right_value) if right_value > 0
      return [true, left_value, 0]
    end

    exploded, left_value, right_value = right.explode(depth + 1)
    if exploded
      left.add_right(left_value) if left_value > 0
      return [true, 0, right_value]
    end

    [false, 0, 0]
  end

  def add_left(value)
    regular? ? self.value += value : left.add_left(value)
  end

  def add_right(value)
    regular? ? self.value += value : right.add_right(value)
  end

  def split
    return left.split || right.split unless regular?

    if value >= 10
      self.left = SnailNumber.new(value: value / 2)
      self.right = SnailNumber.new(value: (value + 1) / 2)
      self.value = nil
      true
    else
      false
    end
  end

  def magnitude
    return value if regular?

    3 * left.magnitude + 2 * right.magnitude
  end

  def deep_copy
    return SnailNumber.new(value: value) if regular?

    SnailNumber.new(left: left.deep_copy, right: right.deep_copy)
  end
end

def parse_snail_number(input)
  input.strip!
  return SnailNumber.new(value: input.to_i) if input[0] != '['

  balance = 0
  split_index = 0
  input[1...-1].chars.each_with_index do |char, i|
    balance += 1 if char == '['
    balance -= 1 if char == ']'
    if char == ',' && balance == 0
      split_index = i + 1
      break
    end
  end

  left = parse_snail_number(input[1...split_index])
  right = parse_snail_number(input[split_index + 1...-1])
  SnailNumber.new(left: left, right: right)
end

snail_numbers = File.readlines('input.txt').map { |line| parse_snail_number(line) }
largest_magnitude = 0

snail_numbers.each_with_index do |a, i|
  snail_numbers.each_with_index do |b, j|
    next if i == j
    sum1 = a.add(b).magnitude
    sum2 = b.add(a).magnitude
    largest_magnitude = [largest_magnitude, sum1, sum2].max
  end
end

puts largest_magnitude