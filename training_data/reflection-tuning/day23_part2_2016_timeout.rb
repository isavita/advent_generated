class Assembunny
  def initialize(instructions)
    @instructions = instructions
    @registers = Hash.new(0)
    @registers['a'] = 12  # Set to 12 for Part Two
    @ip = 0
  end

  def run
    while @ip < @instructions.length
      execute(@instructions[@ip])
      @ip += 1
    end
    @registers['a']
  end

  private

  def execute(instruction)
    op, *args = instruction.split
    case op
    when 'cpy' then cpy(*args)
    when 'inc' then inc(args[0])
    when 'dec' then dec(args[0])
    when 'jnz' then jnz(*args)
    when 'tgl' then tgl(args[0])
    end
  end

  def cpy(x, y)
    @registers[y] = value_of(x) if y =~ /[a-z]/
  end

  def inc(x)
    @registers[x] += 1 if x =~ /[a-z]/
  end

  def dec(x)
    @registers[x] -= 1 if x =~ /[a-z]/
  end

  def jnz(x, y)
    @ip += value_of(y) - 1 if value_of(x) != 0
  end

  def tgl(x)
    target = @ip + value_of(x)
    return if target < 0 || target >= @instructions.length

    inst = @instructions[target].split
    case inst.length
    when 2
      inst[0] = (inst[0] == 'inc' ? 'dec' : 'inc')
    when 3
      inst[0] = (inst[0] == 'jnz' ? 'cpy' : 'jnz')
    end
    @instructions[target] = inst.join(' ')
  end

  def value_of(x)
    x =~ /[a-z]/ ? @registers[x] : x.to_i
  end
end

instructions = File.readlines('input.txt').map(&:chomp)
computer = Assembunny.new(instructions)
result = computer.run
puts result
