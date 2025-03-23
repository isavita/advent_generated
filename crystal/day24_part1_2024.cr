
require "regex"

def solve
  wires = Hash(String, Int32).new
  gates = [] of Tuple(String, String, String, String)
  parsing_wires = true

  File.each_line("input.txt") do |line|
    line = line.strip
    if line.empty?
      parsing_wires = false
      next
    end

    if parsing_wires
      if match = Regex.new("(\\w+):\\s*([01])").match(line)
        wires[match[1].to_s] = match[2].to_s.to_i32
      end
    else
      if match = Regex.new("(\\w+)\\s+(AND|OR|XOR)\\s+(\\w+)\\s+->\\s+(\\w+)").match(line)
        gates << {match[1].to_s, match[2].to_s, match[3].to_s, match[4].to_s}
      end
    end
  end

  remaining_gates = gates
  while !remaining_gates.empty?
    progress = false
    new_remaining_gates = [] of Tuple(String, String, String, String)
    remaining_gates.each do |input1, op, input2, output|
      if wires.has_key?(input1) && wires.has_key?(input2)
        val1 = wires[input1]
        val2 = wires[input2]
        wires[output] = case op
                        when "AND" then (val1 == 1 && val2 == 1) ? 1 : 0
                        when "OR" then (val1 == 1 || val2 == 1) ? 1 : 0
                        when "XOR" then (val1 != val2) ? 1 : 0
                        else 0
                        end
        progress = true
      else
        new_remaining_gates << {input1, op, input2, output}
      end
    end
    return "Cannot evaluate remaining gates due to missing inputs or cyclic dependencies." unless progress
    remaining_gates = new_remaining_gates
  end

  z_wires = Hash(Int32, Int32).new
  wires.each do |wire, val|
    if match = Regex.new("z(\\d+)").match(wire)
      z_wires[match[1].to_s.to_i32] = val
    end
  end

  return "No wires starting with 'z' found." if z_wires.empty?

  binary_string = ""
  z_wires.keys.sort.reverse_each do |i|
    binary_string += z_wires[i].to_s
  end
  binary_string.to_i64(2)
end

puts solve
