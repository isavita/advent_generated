
class SnailNumber
  property value : Int32
  property left : SnailNumber?
  property right : SnailNumber?

  def initialize(@value = -1, @left = nil, @right = nil)
  end

  def regular?
    @left.nil? && @right.nil?
  end

  def add(other : SnailNumber)
    SnailNumber.new(left: self, right: other).reduce
  end

  def reduce
    loop do
      exploded, _, _ = explode(0)
      next if exploded
      break unless split
    end
    self
  end

  def explode(depth)
    return {false, 0, 0} if regular?

    if depth == 4
      left_value = @left.not_nil!.value
      right_value = @right.not_nil!.value
      @left = nil
      @right = nil
      @value = 0
      return {true, left_value, right_value}
    end

    exploded, left_value, right_value = @left.not_nil!.explode(depth + 1)
    if exploded
      if right_value > 0 && @right
        @right.not_nil!.add_left(right_value)
      end
      return {true, left_value, 0}
    end

    exploded, left_value, right_value = @right.not_nil!.explode(depth + 1)
    if exploded
      if left_value > 0 && @left
        @left.not_nil!.add_right(left_value)
      end
      return {true, 0, right_value}
    end

    {false, 0, 0}
  end

  def add_left(value)
    if regular?
      @value += value
    else
      @left.not_nil!.add_left(value)
    end
  end

  def add_right(value)
    if regular?
      @value += value
    else
      @right.not_nil!.add_right(value)
    end
  end

  def split
    if regular?
      if @value >= 10
        @left = SnailNumber.new(value: @value // 2)
        @right = SnailNumber.new(value: (@value + 1) // 2)
        @value = -1
        return true
      end
      return false
    end
    @left.not_nil!.split || @right.not_nil!.split
  end

  def magnitude
    return @value if regular?
    3 * @left.not_nil!.magnitude + 2 * @right.not_nil!.magnitude
  end

  def deep_copy
    return SnailNumber.new(value: @value) if regular?
    SnailNumber.new(left: @left.not_nil!.deep_copy, right: @right.not_nil!.deep_copy)
  end
end

def parse_snail_number(input)
  input = input.strip
  if input[0] != '['
    return SnailNumber.new(value: input.to_i)
  end

  balance = 0
  split_index = 0
  input[1..-2].each_char.with_index do |char, i|
    case char
    when '['
      balance += 1
    when ']'
      balance -= 1
    when ','
      if balance == 0
        split_index = i + 1
        break
      end
    end
    break if split_index != 0
  end

  left = parse_snail_number(input[1...split_index])
  right = parse_snail_number(input[split_index + 1..-2])
  SnailNumber.new(left: left, right: right)
end

file = File.open("input.txt")
snail_numbers = [] of SnailNumber
file.each_line do |line|
  snail_numbers << parse_snail_number(line)
end
file.close

largest_magnitude = 0

snail_numbers.each_with_index do |a, i|
  snail_numbers.each_with_index do |b, j|
    next if i == j

    sum1 = a.deep_copy.add(b.deep_copy).magnitude
    sum2 = b.deep_copy.add(a.deep_copy).magnitude

    largest_magnitude = sum1 if sum1 > largest_magnitude
    largest_magnitude = sum2 if sum2 > largest_magnitude
  end
end

puts largest_magnitude
