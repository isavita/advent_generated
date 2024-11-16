
class SnailNumber
  property value : Int32
  property left : SnailNumber?
  property right : SnailNumber?

  def initialize(@value : Int32 = -1, @left : SnailNumber? = nil, @right : SnailNumber? = nil)
  end

  def regular?
    @left.nil? && @right.nil?
  end

  def add(other : SnailNumber) : SnailNumber
    result = SnailNumber.new(left: self, right: other)
    result.reduce
  end

  def reduce : SnailNumber
    loop do
      exploded, _, _ = explode(0)
      break unless exploded || split
    end
    self
  end

  def explode(depth : Int32) : {Bool, Int32, Int32}
    return {false, 0, 0} if regular?

    if depth == 4
      left_value = @left.not_nil!.value
      right_value = @right.not_nil!.value
      @left = nil
      @right = nil
      @value = 0
      return {true, left_value, right_value}
    end

    if @left
      exploded, left_value, right_value = @left.not_nil!.explode(depth + 1)
      if exploded
        @right.not_nil!.add_left(right_value) if right_value > 0 && @right
        return {true, left_value, 0}
      end
    end

    if @right
      exploded, left_value, right_value = @right.not_nil!.explode(depth + 1)
      if exploded
        @left.not_nil!.add_right(left_value) if left_value > 0 && @left
        return {true, 0, right_value}
      end
    end

    {false, 0, 0}
  end

  def add_left(value : Int32)
    if regular?
      @value += value
    else
      @left.not_nil!.add_left(value)
    end
  end

  def add_right(value : Int32)
    if regular?
      @value += value
    else
      @right.not_nil!.add_right(value)
    end
  end

  def split : Bool
    if regular?
      if @value >= 10
        @left = SnailNumber.new(@value // 2)
        @right = SnailNumber.new((@value + 1) // 2)
        @value = -1
        return true
      end
      return false
    end

    (@left.not_nil!.split || @right.not_nil!.split)
  end

  def magnitude : Int32
    return @value if regular?
    3 * @left.not_nil!.magnitude + 2 * @right.not_nil!.magnitude
  end

  def self.parse(input : String) : SnailNumber
    input = input.strip
    return SnailNumber.new(input.to_i) if input[0] != '['

    balance = 0
    split_index = 0
    input[1...-1].each_char_with_index do |char, i|
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

    left = parse(input[1...split_index])
    right = parse(input[split_index + 1...-1])
    SnailNumber.new(left: left, right: right)
  end
end

def solve
  snail_numbers = File.read_lines("input.txt").map { |line| SnailNumber.parse(line) }
  
  result = snail_numbers[0]
  snail_numbers[1..-1].each do |number|
    result = result.add(number)
  end

  puts result.magnitude
end

solve
