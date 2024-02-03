
def run_intcode(program, inputs)
  memory = program.dup
  ip = 0
  relative_base = 0
  output = []

  get_value = ->(mode, param) do
    case mode
    when 0 then memory[param] || 0
    when 1 then param
    when 2 then memory[relative_base + param] || 0
    else raise "Unknown mode: #{mode}"
    end
  end

  set_value = ->(mode, param, value) do
    case mode
    when 0 then memory[param] = value
    when 2 then memory[relative_base + param] = value
    else raise "Unknown mode for write: #{mode}"
    end
  end

  while memory[ip] != 99
    opcode, modes = memory[ip] % 100, memory[ip].digits.drop(2)
    mode1, mode2, mode3 = modes[0] || 0, modes[1] || 0, modes[2] || 0

    case opcode
    when 1
      set_value.call(mode3, memory[ip + 3], get_value.call(mode1, memory[ip + 1]) + get_value.call(mode2, memory[ip + 2]))
      ip += 4
    when 2
      set_value.call(mode3, memory[ip + 3], get_value.call(mode1, memory[ip + 1]) * get_value.call(mode2, memory[ip + 2]))
      ip += 4
    when 3
      input = inputs.shift
      set_value.call(mode1, memory[ip + 1], input)
      ip += 2
    when 4
      output << get_value.call(mode1, memory[ip + 1])
      ip += 2
    when 5
      ip = get_value.call(mode1, memory[ip + 1]) != 0 ? get_value.call(mode2, memory[ip + 2]) : ip + 3
    when 6
      ip = get_value.call(mode1, memory[ip + 1]) == 0 ? get_value.call(mode2, memory[ip + 2]) : ip + 3
    when 7
      set_value.call(mode3, memory[ip + 3], get_value.call(mode1, memory[ip + 1]) < get_value.call(mode2, memory[ip + 2]) ? 1 : 0)
      ip += 4
    when 8
      set_value.call(mode3, memory[ip + 3], get_value.call(mode1, memory[ip + 1]) == get_value.call(mode2, memory[ip + 2]) ? 1 : 0)
      ip += 4
    when 9
      relative_base += get_value.call(mode1, memory[ip + 1])
      ip += 2
    else
      raise "Unknown opcode: #{opcode}"
    end
  end

  output
end

def affected_points_in_area(program, size)
  affected = 0
  (0...size).each do |y|
    (0...size).each do |x|
      output = run_intcode(program, [x, y])
      affected += 1 if output.first == 1
    end
  end
  affected
end

program = File.read("input.txt").split(",").map(&:to_i)
puts affected_points_in_area(program, 50)
