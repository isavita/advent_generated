
class SnailNumber
  attr_accessor :value, :left, :right

  def initialize(value = nil, left = nil, right = nil)
    @value = value
    @left = left
    @right = right
  end

  def regular?
    @left.nil? && @right.nil?
  end

  def add(other)
    new_number = SnailNumber.new(nil, self, other)
    new_number.reduce
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
    return [false, 0, 0] if regular?

    if depth == 4
      left_value = @left.value
      right_value = @right.value
      @left = nil
      @right = nil
      @value = 0
      return [true, left_value, right_value]
    end

    exploded, left_value, right_value = @left.explode(depth + 1)
    if exploded
      @right.add_left(right_value) if right_value > 0 && @right
      return [true, left_value, 0]
    end

    exploded, left_value, right_value = @right.explode(depth + 1)
    if exploded
      @left.add_right(left_value) if left_value > 0 && @left
      return [true, 0, right_value]
    end

    [false, 0, 0]
  end

  def add_left(value)
    if regular?
      @value += value
    else
      @left.add_left(value)
    end
  end

  def add_right(value)
    if regular?
      @value += value
    else
      @right.add_right(value)
    end
  end

  def split
    if regular?
      if @value >= 10
        @left = SnailNumber.new(@value / 2)
        @right = SnailNumber.new((@value + 1) / 2)
        @value = -1
        true
      else
        false
      end
    else
      @left.split || @right.split
    end
  end

  def magnitude
    return @value if regular?
    3 * @left.magnitude + 2 * @right.magnitude
  end

  def self.parse(input)
    input = input.strip
    if input[0] != '['
      return SnailNumber.new(input.to_i)
    end

    balance = 0
    split_index = 0
    input[1...-1].chars.each_with_index do |char, i|
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

    left = SnailNumber.parse(input[1...split_index])
    right = SnailNumber.parse(input[split_index + 1...-1])
    SnailNumber.new(nil, left, right)
  end
end

snail_numbers = File.readlines("input.txt").map { |line| SnailNumber.parse(line) }

if snail_numbers.empty?
  puts "No snailfish numbers found in the file."
else
  result = snail_numbers.reduce { |acc, sn| acc.add(sn) }
  puts result.magnitude
end
