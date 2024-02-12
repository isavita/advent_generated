
def produces_clock_signal(a : Int32, instructions : Array(String)) : Bool
  registers = {"a" => a, "b" => 0, "c" => 0, "d" => 0}
  last_output = 0
  output_count = 0

  i = 0
  while i < instructions.size
    parts = instructions[i].split(" ")
    case parts[0]
    when "cpy"
      val = parts[1].to_i? || registers[parts[1]]
      registers[parts[2]] = val
    when "inc"
      registers[parts[1]] += 1
    when "dec"
      registers[parts[1]] -= 1
    when "jnz"
      val = parts[1].to_i? || registers[parts[1]]
      if val != 0
        jump = parts[2].to_i
        i += jump
        next
      end
    when "out"
      val = parts[1].to_i? || registers[parts[1]]
      return false if val != 0 && val != 1
      return false if output_count > 0 && val == last_output
      last_output = val
      output_count += 1
      return true if output_count > 50
    end
    i += 1
  end
  false
end

def get_value(s : String, registers : Hash(String, Int32)) : Int32
  s.to_i? || registers[s]
end

instructions = File.read("input.txt").split("\n")

a = 1
loop do
  if produces_clock_signal(a, instructions)
    puts a
    break
  end
  a += 1
end
