class Device
  attr_reader :registers

  def initialize(initial_value = 0)
    @registers = [initial_value, 0, 0, 0, 0, 0]
    @ip = 0
    @ip_register = nil
    @instructions = []
    @ops = {
      'addr' => ->(a, b, c) { @registers[c] = @registers[a] + @registers[b] },
      'addi' => ->(a, b, c) { @registers[c] = @registers[a] + b },
      'mulr' => ->(a, b, c) { @registers[c] = @registers[a] * @registers[b] },
      'muli' => ->(a, b, c) { @registers[c] = @registers[a] * b },
      'banr' => ->(a, b, c) { @registers[c] = @registers[a] & @registers[b] },
      'bani' => ->(a, b, c) { @registers[c] = @registers[a] & b },
      'borr' => ->(a, b, c) { @registers[c] = @registers[a] | @registers[b] },
      'bori' => ->(a, b, c) { @registers[c] = @registers[a] | b },
      'setr' => ->(a, b, c) { @registers[c] = @registers[a] },
      'seti' => ->(a, b, c) { @registers[c] = a },
      'gtir' => ->(a, b, c) { @registers[c] = a > @registers[b] ? 1 : 0 },
      'gtri' => ->(a, b, c) { @registers[c] = @registers[a] > b ? 1 : 0 },
      'gtrr' => ->(a, b, c) { @registers[c] = @registers[a] > @registers[b] ? 1 : 0 },
      'eqir' => ->(a, b, c) { @registers[c] = a == @registers[b] ? 1 : 0 },
      'eqri' => ->(a, b, c) { @registers[c] = @registers[a] == b ? 1 : 0 },
      'eqrr' => ->(a, b, c) { @registers[c] = @registers[a] == @registers[b] ? 1 : 0 }
    }
  end

  def load_program(input)
    input.each_line do |line|
      if line.start_with?('#ip')
        @ip_register = line.split[1].to_i
      else
        @instructions << line.strip.split
      end
    end
  end

  def run
    while @ip >= 0 && @ip < @instructions.length
      @registers[@ip_register] = @ip if @ip_register
      op, *args = @instructions[@ip]
      @ops[op].call(*args.map(&:to_i))
      @ip = @registers[@ip_register] if @ip_register
      @ip += 1
    end
  end
end

def solve_part_one(input)
  device = Device.new
  device.load_program(input)
  device.run
  device.registers[0]
end

def solve_part_two(input)
  device = Device.new(1)
  device.load_program(input)
  device.run
  device.registers[0]
end

# Example usage:
input = File.read('input.txt')
puts "Part One: #{solve_part_one(input)}"
puts "Part Two: #{solve_part_two(input)}"
