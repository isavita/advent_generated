class Coprocessor
  property registers : Hash(Char, Int32)

  def initialize
    @registers = Hash(Char, Int32).new { |h, k| h[k] = 0 }
  end

  def execute(instructions : Array(String))
    mul_count = 0
    i = 0
    while i < instructions.size
      parts = instructions[i].split
      case parts[0]
      when "set"
        @registers[parts[1][0]] = value(parts[2])
      when "sub"
        @registers[parts[1][0]] -= value(parts[2])
      when "mul"
        mul_count += 1
        @registers[parts[1][0]] *= value(parts[2])
      when "jnz"
        if value(parts[1]) != 0
          i += value(parts[2]) - 1
        end
      end
      i += 1
    end
    mul_count
  end

  private def value(param : String) : Int32
    if param.to_i? != nil
      param.to_i
    else
      @registers[param[0]]
    end
  end
end

instructions = File.read_lines("input.txt")
coprocessor = Coprocessor.new
puts coprocessor.execute(instructions)