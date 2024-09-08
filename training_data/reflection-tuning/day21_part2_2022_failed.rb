class Monkey
  attr_reader :name, :operation, :left, :right, :value

  def initialize(name, operation, left, right, value)
    @name = name
    @operation = operation
    @left = left
    @right = right
    @value = value
  end

  def evaluate(monkeys, humn_value = nil)
    return humn_value if name == 'humn' && !humn_value.nil?
    return value if value

    left_val = monkeys[left].evaluate(monkeys, humn_value)
    right_val = monkeys[right].evaluate(monkeys, humn_value)

    case operation
    when '+' then left_val + right_val
    when '-' then left_val - right_val
    when '*' then left_val * right_val
    when '/' then left_val / right_val
    when '=' then left_val == right_val
    end
  end
end

def parse_input(input)
  monkeys = {}
  input.each_line do |line|
    name, job = line.strip.split(': ')
    if job =~ /^\d+$/
      monkeys[name] = Monkey.new(name, nil, nil, nil, job.to_i)
    else
      left, op, right = job.split
      monkeys[name] = Monkey.new(name, op, left, right, nil)
    end
  end
  monkeys
end

def solve_part1(monkeys)
  monkeys['root'].evaluate(monkeys).to_i
end

def solve_part2(monkeys)
  root = monkeys['root']
  monkeys['root'] = Monkey.new('root', '=', root.left, root.right, nil)

  low = 0
  high = 10**15

  while low <= high
    mid = (low + high) / 2
    left_val = monkeys[root.left].evaluate(monkeys, mid)
    right_val = monkeys[root.right].evaluate(monkeys, mid)

    if left_val == right_val
      return mid
    elsif left_val < right_val
      low = mid + 1
    else
      high = mid - 1
    end
  end
end

input = File.read('input.txt')
monkeys = parse_input(input)

puts "Part 1: #{solve_part1(monkeys)}"
puts "Part 2: #{solve_part2(monkeys)}"
