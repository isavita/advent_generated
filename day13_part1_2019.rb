
intcode = File.read('input.txt').chomp.split(',').map(&:to_i)

def run_intcode(intcode, input)
  i = 0
  relative_base = 0
  output = []
  
  while i < intcode.length
    opcode = intcode[i] % 100
    modes = intcode[i] / 100
    
    param1 = get_value(intcode, i+1, modes % 10, relative_base)
    param2 = get_value(intcode, i+2, modes / 10 % 10, relative_base)
    param3 = get_address(intcode, i+3, modes / 100 % 10, relative_base)
    
    case opcode
    when 1
      intcode[param3] = param1 + param2
      i += 4
    when 2
      intcode[param3] = param1 * param2
      i += 4
    when 3
      intcode[get_address(intcode, i+1, modes % 10, relative_base)] = input
      i += 2
    when 4
      output << param1
      i += 2
    when 5
      i = param1 != 0 ? param2 : i + 3
    when 6
      i = param1 == 0 ? param2 : i + 3
    when 7
      intcode[param3] = param1 < param2 ? 1 : 0
      i += 4
    when 8
      intcode[param3] = param1 == param2 ? 1 : 0
      i += 4
    when 9
      relative_base += param1
      i += 2
    when 99
      break
    end
  end
  
  return output
end

def get_value(intcode, i, mode, relative_base)
  case mode
  when 0
    return intcode[intcode[i]] || 0
  when 1
    return intcode[i] || 0
  when 2
    return intcode[intcode[i] + relative_base] || 0
  end
end

def get_address(intcode, i, mode, relative_base)
  case mode
  when 0
    return intcode[i] || 0
  when 2
    return intcode[i] + relative_base || 0
  end
end

intcode[0] = 2 # Set memory address 0 to 2 to play for free

output = run_intcode(intcode, 0)

blocks = output.each_slice(3).count { |x, y, tile| tile == 2 }

puts blocks
