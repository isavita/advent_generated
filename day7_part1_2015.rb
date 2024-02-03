
def evaluate(instruction, wires)
  operation, result = instruction.split(' -> ')
  case operation
  when /^\d+$/
    wires[result] = operation.to_i
  when /^NOT (\w+)$/
    wires[result] = ~wires[$1] & 0xFFFF
  when /^(\w+) AND (\w+)$/
    wires[result] = wires[$1] & wires[$2]
  when /^(\w+) OR (\w+)$/
    wires[result] = wires[$1] | wires[$2]
  when /^(\w+) LSHIFT (\d+)$/
    wires[result] = (wires[$1] << $2.to_i) & 0xFFFF
  when /^(\w+) RSHIFT (\d+)$/
    wires[result] = wires[$1] >> $2.to_i
  else
    if operation =~ /^\w+$/
      wires[result] = wires[operation]
    end
  end
end

def solve(instructions)
  wires = Hash.new { |h, k| h[k] = (k =~ /^\d+$/ ? k.to_i : nil) }
  until instructions.empty?
    instructions.reject! do |instruction|
      evaluate(instruction, wires) unless instruction.split(' -> ').first.split.any? { |part| part =~ /[a-z]/ && wires[part].nil? }
    end
  end
  wires['a']
end

instructions = File.readlines('input.txt', chomp: true)
puts solve(instructions)
