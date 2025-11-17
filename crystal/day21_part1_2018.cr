
class OpcodeComputer
  @registers : Array(Int64)
  @instructions : Array(Instruction)
  @instruction_pointer : Int32

  def initialize(@instructions, @instruction_pointer)
    @registers = Array.new(6, 0_i64)
  end

  def tick : Bool
    ip = @registers[@instruction_pointer]
    return true if ip >= @instructions.size
    inst = @instructions[ip]
    case inst.name
    when "addr" then @registers[inst.c] = @registers[inst.a] + @registers[inst.b]
    when "addi" then @registers[inst.c] = @registers[inst.a] + inst.b
    when "mulr" then @registers[inst.c] = @registers[inst.a] * @registers[inst.b]
    when "muli" then @registers[inst.c] = @registers[inst.a] * inst.b
    when "banr" then @registers[inst.c] = @registers[inst.a] & @registers[inst.b]
    when "bani" then @registers[inst.c] = @registers[inst.a] & inst.b
    when "borr" then @registers[inst.c] = @registers[inst.a] | @registers[inst.b]
    when "bori" then @registers[inst.c] = @registers[inst.a] | inst.b
    when "setr" then @registers[inst.c] = @registers[inst.a]
    when "seti" then @registers[inst.c] = inst.a
    when "gtir" then @registers[inst.c] = inst.a > @registers[inst.b] ? 1_i64 : 0_i64
    when "gtri" then @registers[inst.c] = @registers[inst.a] > inst.b ? 1_i64 : 0_i64
    when "gtrr" then @registers[inst.c] = @registers[inst.a] > @registers[inst.b] ? 1_i64 : 0_i64
    when "eqir" then @registers[inst.c] = inst.a == @registers[inst.b] ? 1_i64 : 0_i64
    when "eqri" then @registers[inst.c] = @registers[inst.a] == inst.b ? 1_i64 : 0_i64
    when "eqrr" then @registers[inst.c] = @registers[inst.a] == @registers[inst.b] ? 1_i64 : 0_i64
    end
    @registers[@instruction_pointer] += 1
    @registers[@instruction_pointer] >= @instructions.size
  end
end

record Instruction, name : String, a : Int64, b : Int64, c : Int64

input = File.read("input.txt").lines
ip = input[0].split[1].to_i
instructions = input[1..].reject(&.empty?).map do |line|
  parts = line.split
  Instruction.new(parts[0], parts[1].to_i64, parts[2].to_i64, parts[3].to_i64)
end

comp = OpcodeComputer.new(instructions, ip)
until comp.tick
  if comp.@registers[ip] == 28
    puts comp.@registers[5]
    exit
  end
end
