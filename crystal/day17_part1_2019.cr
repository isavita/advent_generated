
struct Machine
  getter data : Hash(Int64, Int64)
  getter ip : Int64
  getter input : Array(Int64)
  getter output : Array(Int64)
  property relbase : Int64

  def initialize(@data, @ip, @input, @output, @relbase)
  end

  def decode(n)
    op = n % 100
    modes = Array(Int32).new
    n //= 100
    3.times do
      modes << (n % 10).to_i32
      n //= 10
    end
    {op, modes}
  end

  def get(i, mode)
    case mode
    when 1
      data.fetch(i, 0_i64)
    when 0
      data.fetch(data.fetch(i, 0_i64), 0_i64)
    when 2
      data.fetch(relbase + data.fetch(i, 0_i64), 0_i64)
    else
      0_i64
    end
  end

  def set(i, mode, val)
    case mode
    when 0
      data[data.fetch(i, 0_i64)] = val
    when 2
      data[relbase + data.fetch(i, 0_i64)] = val
    end
  end

  def step
    op, modes = decode(data.fetch(ip, 0_i64))
    case op
    when 1, 2
      val1 = get(ip + 1, modes[0])
      val2 = get(ip + 2, modes[1])
      if op == 1
        set(ip + 3, modes[2], val1 + val2)
      else
        set(ip + 3, modes[2], val1 * val2)
      end
      @ip += 4
    when 3
      set(ip + 1, modes[0], input.shift)
      @ip += 2
    when 4
      output << get(ip + 1, modes[0])
      @ip += 2
    when 5, 6
      val = get(ip + 1, modes[0])
      if (op == 5 && val != 0) || (op == 6 && val == 0)
        @ip = get(ip + 2, modes[1])
      else
        @ip += 3
      end
    when 7, 8
      val1 = get(ip + 1, modes[0])
      val2 = get(ip + 2, modes[1])
      if (op == 7 && val1 < val2) || (op == 8 && val1 == val2)
        set(ip + 3, modes[2], 1_i64)
      else
        set(ip + 3, modes[2], 0_i64)
      end
      @ip += 4
    when 9
      @relbase += get(ip + 1, modes[0])
      @ip += 2
    when 99
      return false
    end
    true
  end

  def run
    while step
    end
    output
  end
end

input = File.read("input.txt")
program = input.split(',').map(&.to_i64)
data = Hash(Int64, Int64).new(0_i64)
program.each_with_index { |v, i| data[i.to_i64] = v }
machine = Machine.new(data, 0_i64, [] of Int64, [] of Int64, 0_i64)
out = machine.run

scaffolding = Set(Int32).new
robot_x = 0
robot_y = 0
y = 0
x = 0
out.each do |v|
  c = v.chr
  if c == '\n'
    y += 1
    x = 0
  else
    if c == '#'
      scaffolding << y * 1000 + x
    elsif "^v<>".includes?(c)
      robot_x = x
      robot_y = y
      scaffolding << y * 1000 + x
    end
    x += 1
  end
end

sum = 0
scaffolding.each do |p|
  x = p % 1000
  y = p // 1000
  count = 0
  [-1, 1, -1000, 1000].each do |n|
    count += 1 if scaffolding.includes?(p + n)
  end
  sum += x * y if count == 4
end
puts sum
