
intcode = File.read('input.txt').chomp.split(',').map(&:to_i)
intcode += Array.new(1000, 0)

def run_intcode(intcode, input)
  i = 0
  relative_base = 0
  output = nil

  while i < intcode.length
    opcode = intcode[i] % 100
    modes = intcode[i] / 100

    get_value = ->(param, mode) {
      case mode
      when 0
        intcode[param]
      when 1
        param
      when 2
        intcode[param + relative_base]
      end
    }

    set_value = ->(param, mode, value) {
      case mode
      when 0
        intcode[param] = value
      when 2
        intcode[param + relative_base] = value
      end
    }

    case opcode
    when 1
      val1 = get_value.call(intcode[i + 1], modes % 10)
      val2 = get_value.call(intcode[i + 2], modes / 10 % 10)
      set_value.call(intcode[i + 3], modes / 100, val1 + val2)
      i += 4
    when 2
      val1 = get_value.call(intcode[i + 1], modes % 10)
      val2 = get_value.call(intcode[i + 2], modes / 10 % 10)
      set_value.call(intcode[i + 3], modes / 100, val1 * val2)
      i += 4
    when 3
      set_value.call(intcode[i + 1], modes % 10, input)
      i += 2
    when 4
      output = get_value.call(intcode[i + 1], modes % 10)
      i += 2
    when 5
      val1 = get_value.call(intcode[i + 1], modes % 10)
      val2 = get_value.call(intcode[i + 2], modes / 10 % 10)
      i = val1 != 0 ? val2 : i + 3
    when 6
      val1 = get_value.call(intcode[i + 1], modes % 10)
      val2 = get_value.call(intcode[i + 2], modes / 10 % 10)
      i = val1 == 0 ? val2 : i + 3
    when 7
      val1 = get_value.call(intcode[i + 1], modes % 10)
      val2 = get_value.call(intcode[i + 2], modes / 10 % 10)
      set_value.call(intcode[i + 3], modes / 100, val1 < val2 ? 1 : 0)
      i += 4
    when 8
      val1 = get_value.call(intcode[i + 1], modes % 10)
      val2 = get_value.call(intcode[i + 2], modes / 10 % 10)
      set_value.call(intcode[i + 3], modes / 100, val1 == val2 ? 1 : 0)
      i += 4
    when 9
      relative_base += get_value.call(intcode[i + 1], modes % 10)
      i += 2
    when 99
      break
    end
  end

  output
end

puts run_intcode(intcode, 1)
