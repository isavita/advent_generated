class AssembunnyInterpreter
  def initialize(instructions)
    @instructions = instructions
    @registers = {'a' => 0, 'b' => 0, 'c' => 0, 'd' => 0}
    @pc = 0
  end

  def run(initial_a = 12)
    @registers['a'] = initial_a
    while @pc < @instructions.length
      optimize_multiplication
      execute(@instructions[@pc])
    end
    @registers['a']
  end

  private

  def execute(instruction)
    op, *args = instruction.split
    case op
    when 'cpy'
      value = get_value(args[0])
      @registers[args[1]] = value if @registers.key?(args[1])
      @pc += 1
    when 'inc'
      @registers[args[0]] += 1 if @registers.key?(args[0])
      @pc += 1
    when 'dec'
      @registers[args[0]] -= 1 if @registers.key?(args[0])
      @pc += 1
    when 'jnz'
      value = get_value(args[0])
      @pc += value != 0 ? get_value(args[1]) : 1
    when 'tgl'
      target = @pc + get_value(args[0])
      if target >= 0 && target < @instructions.length
        toggle_instruction(target)
      end
      @pc += 1
    end
  end

  def get_value(arg)
    @registers.key?(arg) ? @registers[arg] : arg.to_i
  end

  def toggle_instruction(index)
    instruction = @instructions[index].split
    case instruction.length
    when 2
      instruction[0] = instruction[0] == 'inc' ? 'dec' : 'inc'
    when 3
      instruction[0] = instruction[0] == 'jnz' ? 'cpy' : 'jnz'
    end
    @instructions[index] = instruction.join(' ')
  end

  def optimize_multiplication
    if @pc + 5 < @instructions.length
      if @instructions[@pc..@pc+5] == [
        "cpy b c",
        "inc a",
        "dec c",
        "jnz c -2",
        "dec d",
        "jnz d -5"
      ]
        @registers['a'] += @registers['b'] * @registers['d']
        @registers['c'] = 0
        @registers['d'] = 0
        @pc += 6
      end
    end
  end
end

# Read input from file
instructions = File.readlines('input.txt', chomp: true)

# Create interpreter and run program
interpreter = AssembunnyInterpreter.new(instructions)
result = interpreter.run

puts "Value sent to the safe (Part 2): #{result}"
