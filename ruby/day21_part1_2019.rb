
def run_intcode(program, input)
  memory = program.dup
  i = 0
  relative_base = 0
  output = []

  get_param = lambda do |mode, offset|
    case mode
    when 0
      memory[memory[i + offset].to_i] || 0
    when 1
      memory[i + offset] || 0
    when 2
      memory[memory[i + offset].to_i + relative_base] || 0
    end
  end

  set_param = lambda do |mode, offset, value|
    case mode
    when 0
      memory[memory[i + offset].to_i] = value
    when 2
      memory[memory[i + offset].to_i + relative_base] = value
    end
  end

  loop do
    instruction = memory[i].to_s.rjust(5, '0')
    opcode = instruction[-2..-1].to_i
    mode1 = instruction[-3].to_i
    mode2 = instruction[-4].to_i
    mode3 = instruction[-5].to_i

    case opcode
    when 1
      param1 = get_param.call(mode1, 1)
      param2 = get_param.call(mode2, 2)
      set_param.call(mode3, 3, param1 + param2)
      i += 4
    when 2
      param1 = get_param.call(mode1, 1)
      param2 = get_param.call(mode2, 2)
      set_param.call(mode3, 3, param1 * param2)
      i += 4
    when 3
      set_param.call(mode1, 1, input.shift)
      i += 2
    when 4
      output << get_param.call(mode1, 1)
      i += 2
    when 5
      param1 = get_param.call(mode1, 1)
      param2 = get_param.call(mode2, 2)
      i = param1 != 0 ? param2 : i + 3
    when 6
      param1 = get_param.call(mode1, 1)
      param2 = get_param.call(mode2, 2)
      i = param1 == 0 ? param2 : i + 3
    when 7
      param1 = get_param.call(mode1, 1)
      param2 = get_param.call(mode2, 2)
      set_param.call(mode3, 3, param1 < param2 ? 1 : 0)
      i += 4
    when 8
      param1 = get_param.call(mode1, 1)
      param2 = get_param.call(mode2, 2)
      set_param.call(mode3, 3, param1 == param2 ? 1 : 0)
      i += 4
    when 9
      relative_base += get_param.call(mode1, 1)
      i += 2
    when 99
      break
    end
  end
  output
end

program = File.read('input.txt').split(',').map(&:to_i)

springscript = [
  "NOT A J",
  "NOT B T",
  "OR T J",
  "NOT C T",
  "OR T J",
  "AND D J",
  "WALK"
].join("\n") + "\n"

input = springscript.chars.map(&:ord)
output = run_intcode(program, input)

if output.last > 255
  puts output.last
else
  puts output.map(&:chr).join
end
