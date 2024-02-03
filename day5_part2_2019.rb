
def get_value(mode, index, codes)
  mode == 0 ? codes[codes[index]] : codes[index]
end

def run_intcode(codes)
  i = 0
  while codes[i] != 99
    opcode = codes[i] % 100
    mode1 = (codes[i] / 100) % 10
    mode2 = (codes[i] / 1000) % 10
    mode3 = (codes[i] / 10000) % 10
    case opcode
    when 1
      codes[codes[i + 3]] = get_value(mode1, i + 1, codes) + get_value(mode2, i + 2, codes)
      i += 4
    when 2
      codes[codes[i + 3]] = get_value(mode1, i + 1, codes) * get_value(mode2, i + 2, codes)
      i += 4
    when 3
      codes[codes[i + 1]] = 5 # Input value
      i += 2
    when 4
      puts get_value(mode1, i + 1, codes)
      i += 2
    when 5
      if get_value(mode1, i + 1, codes) != 0
        i = get_value(mode2, i + 2, codes)
      else
        i += 3
      end
    when 6
      if get_value(mode1, i + 1, codes) == 0
        i = get_value(mode2, i + 2, codes)
      else
        i += 3
      end
    when 7
      codes[codes[i + 3]] = get_value(mode1, i + 1, codes) < get_value(mode2, i + 2, codes) ? 1 : 0
      i += 4
    when 8
      codes[codes[i + 3]] = get_value(mode1, i + 1, codes) == get_value(mode2, i + 2, codes) ? 1 : 0
      i += 4
    else
      puts "Unknown opcode: #{opcode}"
      break
    end
  end
end

codes = File.read("input.txt").split(",").map(&:to_i)
run_intcode(codes)
