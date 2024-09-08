class VirtualMachine
  def initialize(ip_register, instructions)
    @registers = [0] * 6
    @ip = 0
    @ip_register = ip_register
    @instructions = instructions
    @opcodes = {
      'seti' => ->(a, b, c) { @registers[c] = a },
      'setr' => ->(a, b, c) { @registers[c] = @registers[a] },
      'addi' => ->(a, b, c) { @registers[c] = @registers[a] + b },
      'addr' => ->(a, b, c) { @registers[c] = @registers[a] + @registers[b] },
      'muli' => ->(a, b, c) { @registers[c] = @registers[a] * b },
      'mulr' => ->(a, b, c) { @registers[c] = @registers[a] * @registers[b] },
      'bani' => ->(a, b, c) { @registers[c] = @registers[a] & b },
      'banr' => ->(a, b, c) { @registers[c] = @registers[a] & @registers[b] },
      'bori' => ->(a, b, c) { @registers[c] = @registers[a] | b },
      'borr' => ->(a, b, c) { @registers[c] = @registers[a] | @registers[b] },
      'gtir' => ->(a, b, c) { @registers[c] = a > @registers[b] ? 1 : 0 },
      'gtri' => ->(a, b, c) { @registers[c] = @registers[a] > b ? 1 : 0 },
      'gtrr' => ->(a, b, c) { @registers[c] = @registers[a] > @registers[b] ? 1 : 0 },
      'eqir' => ->(a, b, c) { @registers[c] = a == @registers[b] ? 1 : 0 },
      'eqri' => ->(a, b, c) { @registers[c] = @registers[a] == b ? 1 : 0 },
      'eqrr' => ->(a, b, c) { @registers[c] = @registers[a] == @registers[b] ? 1 : 0 }
    }
  end

  def run
    while @ip >= 0 && @ip < @instructions.length
      @registers[@ip_register] = @ip
      opcode, *params = @instructions[@ip]
      @opcodes[opcode].call(*params)
      @ip = @registers[@ip_register]
      @ip += 1
    end
    @registers[0]
  end
end

# Parse input
lines = File.readlines('input.txt', chomp: true)
ip_register = lines.shift.split.last.to_i
instructions = lines.map { |line| line.split.map { |x| x =~ /\d+/ ? x.to_i : x } }

# Create and run the virtual machine
vm = VirtualMachine.new(ip_register, instructions)
result = vm.run

puts result
