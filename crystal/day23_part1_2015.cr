class TuringLock
  def initialize
    @registers = {"a" => 0, "b" => 0}
    @instructions = [] of String
  end

  def load_instructions(file)
    File.open(file) do |f|
      f.each_line { |line| @instructions << line.chomp }
    end
  end

  def execute
    pc = 0
    while pc < @instructions.size
      instruction = @instructions[pc]
      case instruction
      when /hlf (\w+)/
        @registers[$1] = (@registers[$1] / 2).to_i
        pc += 1
      when /tpl (\w+)/
        @registers[$1] *= 3
        pc += 1
      when /inc (\w+)/
        @registers[$1] += 1
        pc += 1
      when /jmp ([+-]\d+)/
        pc += $1.to_i
      when /jie (\w+), ([+-]\d+)/
        pc += $1 == "a" && @registers["a"].even? ? $2.to_i : 1
      when /jio (\w+), ([+-]\d+)/
        pc += $1 == "a" && @registers["a"] == 1 ? $2.to_i : 1
      end
    end
  end

  def result
    @registers["b"]
  end
end

lock = TuringLock.new
lock.load_instructions("input.txt")
lock.execute
puts lock.result