
def run_intcode(program : Array(Int64), input : Int64) : Int64
  memory = program.dup
  memory.concat(Array.new(10000, 0_i64))
  pc = 0
  relative_base = 0
  output = 0_i64

  loop do
    instruction = memory[pc]
    opcode = instruction % 100
    mode1 = (instruction // 100) % 10
    mode2 = (instruction // 1000) % 10
    mode3 = (instruction // 10000) % 10

    case opcode
    when 1, 2, 7, 8
      param1 = mode1 == 1 ? memory[pc + 1] : (mode1 == 2 ? memory[memory[pc + 1] + relative_base] : memory[memory[pc + 1]])
      param2 = mode2 == 1 ? memory[pc + 2] : (mode2 == 2 ? memory[memory[pc + 2] + relative_base] : memory[memory[pc + 2]])
      dest = mode3 == 2 ? memory[pc + 3] + relative_base : memory[pc + 3]
      if opcode == 1
        memory[dest] = param1 + param2
      elsif opcode == 2
        memory[dest] = param1 * param2
      elsif opcode == 7
        memory[dest] = param1 < param2 ? 1 : 0
      elsif opcode == 8
        memory[dest] = param1 == param2 ? 1 : 0
      end
      pc += 4
    when 3
      dest = mode1 == 2 ? memory[pc + 1] + relative_base : memory[pc + 1]
      memory[dest] = input
      pc += 2
    when 4
      param1 = mode1 == 1 ? memory[pc + 1] : (mode1 == 2 ? memory[memory[pc + 1] + relative_base] : memory[memory[pc + 1]])
      output = param1
      pc += 2
    when 5, 6
      param1 = mode1 == 1 ? memory[pc + 1] : (mode1 == 2 ? memory[memory[pc + 1] + relative_base] : memory[memory[pc + 1]])
      param2 = mode2 == 1 ? memory[pc + 2] : (mode2 == 2 ? memory[memory[pc + 2] + relative_base] : memory[memory[pc + 2]])
      if (opcode == 5 && param1 != 0) || (opcode == 6 && param1 == 0)
        pc = param2
      else
        pc += 3
      end
    when 9
      param1 = mode1 == 1 ? memory[pc + 1] : (mode1 == 2 ? memory[memory[pc + 1] + relative_base] : memory[memory[pc + 1]])
      relative_base += param1
      pc += 2
    when 99
      break
    else
      raise "Unknown opcode: #{opcode}"
    end
  end

  output
end

program = File.read("input.txt").strip.split(',').map(&.to_i64)

puts "BOOST keycode: #{run_intcode(program, 1)}"
puts "Distress signal coordinates: #{run_intcode(program, 2)}"
