class Device
  attr_reader :registers, :ip, :instructions

  def initialize(ip_register, instructions)
    @registers = [0] * 6
    @ip_register = ip_register
    @instructions = instructions
    @ip = 0
  end

  def execute(instruction)
    op, a, b, c = instruction
    case op
    when 'addr' then @registers[c] = @registers[a] + @registers[b]
    when 'addi' then @registers[c] = @registers[a] + b
    when 'mulr' then @registers[c] = @registers[a] * @registers[b]
    when 'muli' then @registers[c] = @registers[a] * b
    when 'banr' then @registers[c] = @registers[a] & @registers[b]
    when 'bani' then @registers[c] = @registers[a] & b
    when 'borr' then @registers[c] = @registers[a] | @registers[b]
    when 'bori' then @registers[c] = @registers[a] | b
    when 'setr' then @registers[c] = @registers[a]
    when 'seti' then @registers[c] = a
    when 'gtir' then @registers[c] = a > @registers[b] ? 1 : 0
    when 'gtri' then @registers[c] = @registers[a] > b ? 1 : 0
    when 'gtrr' then @registers[c] = @registers[a] > @registers[b] ? 1 : 0
    when 'eqir' then @registers[c] = a == @registers[b] ? 1 : 0
    when 'eqri' then @registers[c] = @registers[a] == b ? 1 : 0
    when 'eqrr' then @registers[c] = @registers[a] == @registers[b] ? 1 : 0
    end
  end

  def run(max_instructions = Float::INFINITY)
    instruction_count = 0
    while @ip >= 0 && @ip < @instructions.length && instruction_count < max_instructions
      @registers[@ip_register] = @ip
      execute(@instructions[@ip])
      @ip = @registers[@ip_register]
      @ip += 1
      instruction_count += 1
    end
    instruction_count
  end
end

def parse_input(input)
  lines = input.split("\n")
  ip_register = lines.shift.split.last.to_i
  instructions = lines.map { |line| line.split.map { |x| x =~ /\d+/ ? x.to_i : x } }
  [ip_register, instructions]
end

def find_min_register_0(ip_register, instructions)
  (0..Float::INFINITY).each do |r0|
    device = Device.new(ip_register, instructions)
    device.registers[0] = r0
    return r0 if device.run < Float::INFINITY
  end
end

def find_max_register_0(ip_register, instructions)
  low, high = 0, 10_000_000
  result = 0
  while low <= high
    mid = (low + high) / 2
    device = Device.new(ip_register, instructions)
    device.registers[0] = mid
    if device.run < Float::INFINITY
      result = mid
      low = mid + 1
    else
      high = mid - 1
    end
  end
  result
end

input = File.read('input.txt')
ip_register, instructions = parse_input(input)

puts "Part 1: #{find_min_register_0(ip_register, instructions)}"
puts "Part 2: #{find_max_register_0(ip_register, instructions)}"
