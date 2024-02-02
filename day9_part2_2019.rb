
intcode = File.read('input.txt').split(',').map(&:to_i)
def run_intcode(intcode, input)
  i = 0
  relative_base = 0
  output = nil
  while i < intcode.length
    opcode = intcode[i] % 100
    modes = intcode[i] / 100
    param1 = intcode[i + 1]
    param2 = intcode[i + 2]
    param3 = intcode[i + 3]
    mode1 = modes % 10
    mode2 = (modes / 10) % 10
    mode3 = (modes / 100) % 10
    val1 = mode1 == 1 ? param1 : (mode1 == 2 ? intcode[param1 + relative_base] : intcode[param1])
    val2 = mode2 == 1 ? param2 : (mode2 == 2 ? intcode[param2 + relative_base] : intcode[param2])
    case opcode
    when 1
      intcode[param3 + (mode3 == 2 ? relative_base : 0)] = val1 + val2
      i += 4
    when 2
      intcode[param3 + (mode3 == 2 ? relative_base : 0)] = val1 * val2
      i += 4
    when 3
      intcode[param1 + (mode1 == 2 ? relative_base : 0)] = input
      i += 2
    when 4
      output = val1
      i += 2
    when 5
      i = val1 != 0 ? val2 : i + 3
    when 6
      i = val1 == 0 ? val2 : i + 3
    when 7
      intcode[param3 + (mode3 == 2 ? relative_base : 0)] = val1 < val2 ? 1 : 0
      i += 4
    when 8
      intcode[param3 + (mode3 == 2 ? relative_base : 0)] = val1 == val2 ? 1 : 0
      i += 4
    when 9
      relative_base += val1
      i += 2
    when 99
      break
    end
  end
  output
end

puts run_intcode(intcode, 2)
