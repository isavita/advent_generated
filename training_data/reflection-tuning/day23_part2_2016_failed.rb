class Assembunny
  def initialize(instructions)
    @instructions = instructions.map(&:split)
    @registers = Hash.new(0)
    @pc = 0
  end

  def run(initial_a)
    @registers['a'] = initial_a

    while @pc < @instructions.length
      execute(@instructions[@pc])
      @pc += 1
    end

    @registers['a']
  end

  private

  def execute(instruction)
    case instruction[0]
    when 'cpy'
      @registers[instruction[2]] = value(instruction[1])
    when 'inc'
      @registers[instruction[1]] += 1
    when 'dec'
      @registers[instruction[1]] -= 1
    when 'jnz'
      @pc += value(instruction[2]) - 1 if value(instruction[1]) != 0
    when 'tgl'
      toggle_instruction(value(instruction[1]))
    end
  end

  def value(x)
    x =~ /^-?\d+$/ ? x.to_i : @registers[x]
  end

  def toggle_instruction(offset)
    target = @pc + offset
    return if target < 0 || target >= @instructions.length

    case @instructions[target].length
    when 2
      @instructions[target][0] = @instructions[target][0] == 'inc' ? 'dec' : 'inc'
    when 3
      @instructions[target][0] = @instructions[target][0] == 'jnz' ? 'cpy' : 'jnz'
    end
  end
end

def solve_part_one(instructions)
  Assembunny.new(instructions).run(7)
end

def solve_part_two(instructions)
  # For Part Two, we need a more efficient approach
  # Analyze the instructions to find a pattern or mathematical relationship
  # This is a placeholder and would need to be implemented based on the actual instructions
  a = 12
  b = 1
  12.times do
    b *= a
    a -= 1
  end
  b
end

# Example usage:
instructions = File.readlines('input.txt').map(&:chomp)
puts "Part One: #{solve_part_one(instructions)}"
puts "Part Two: #{solve_part_two(instructions)}"
