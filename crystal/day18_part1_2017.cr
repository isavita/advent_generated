
class Duet
  @registers = Hash(Char, Int64).new(0_i64)
  @last_sound = 0_i64
  @pc = 0

  def initialize(instructions : Array(String))
    @instructions = instructions
  end

  def get_value(x : String) : Int64
    if x =~ /^-?\d+$/
      x.to_i64
    else
      @registers[x[0]]
    end
  end

  def run
    while 0 <= @pc < @instructions.size
      parts = @instructions[@pc].split
      
      case parts[0]
      when "snd"
        @last_sound = get_value(parts[1])
      when "set"
        @registers[parts[1][0]] = get_value(parts[2])
      when "add"
        @registers[parts[1][0]] += get_value(parts[2])
      when "mul"
        @registers[parts[1][0]] *= get_value(parts[2])
      when "mod"
        @registers[parts[1][0]] %= get_value(parts[2])
      when "rcv"
        return @last_sound if get_value(parts[1]) != 0
      when "jgz"
        @pc += get_value(parts[2]) - 1 if get_value(parts[1]) > 0
      end

      @pc += 1
    end
  end
end

# Read instructions from input file
instructions = File.read_lines("input.txt")

# Run the Duet program
duet = Duet.new(instructions)
recovered_frequency = duet.run

puts "Recovered frequency: #{recovered_frequency}"
