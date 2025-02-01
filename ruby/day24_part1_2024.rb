
def solve
  wires = {}
  gates = []
  wire_regex = /^(\w+):\s*([01])$/
  gate_regex = /^(\w+)\s+(AND|OR|XOR)\s+(\w+)\s+->\s+(\w+)$/

  parsing_wires = true
  File.foreach("input.txt") do |line|
    line = line.strip
    if line.empty?
      parsing_wires = false
      next
    end

    if parsing_wires
      match = line.match(wire_regex)
      if match
        wires[match[1]] = match[2].to_i
      else
        puts "Invalid wire definition: #{line}"
        return
      end
    else
       match = line.match(gate_regex)
       if match
          gates << { input1: match[1], operation: match[2], input2: match[3], output: match[4] }
       else
          puts "Invalid gate definition: #{line}"
          return
       end
    end
  end

  remaining_gates = gates
  while remaining_gates.any?
    progress = false
    new_remaining_gates = []
    remaining_gates.each do |gate|
      val1 = wires[gate[:input1]]
      val2 = wires[gate[:input2]]
      if val1 && val2
        case gate[:operation]
        when "AND"
            output_val = (val1 == 1 && val2 == 1) ? 1 : 0
        when "OR"
            output_val = (val1 == 1 || val2 == 1) ? 1 : 0
        when "XOR"
            output_val = (val1 != val2) ? 1 : 0
        end
        wires[gate[:output]] = output_val
        progress = true
      else
        new_remaining_gates << gate
      end
    end
      unless progress
        puts "Cannot evaluate remaining gates due to missing inputs or cyclic dependencies."
        return
      end
    remaining_gates = new_remaining_gates
  end

  z_wires = {}
  z_regex = /^z(\d+)$/
  wires.each do |wire, val|
    match = wire.match(z_regex)
    if match
      z_wires[match[1].to_i] = val
    end
  end

  if z_wires.empty?
    puts "No wires starting with 'z' found."
    return
  end

  binary_string = z_wires.sort.reverse.map { |_, bit| bit.to_s }.join
  puts binary_string.to_i(2)
end

solve
