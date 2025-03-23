
require "set"

def intcode(program : Array(Int32), input : Array(Int32)) : Array(Int32)
  memory = program.dup
  input_index = 0
  output = [] of Int32
  instruction_pointer = 0

  loop do
    opcode = memory[instruction_pointer] % 100
    mode1 = (memory[instruction_pointer] // 100) % 10
    mode2 = (memory[instruction_pointer] // 1000) % 10
    mode3 = (memory[instruction_pointer] // 10000) % 10

    case opcode
    when 1
      param1 = mode1 == 0 ? memory[memory[instruction_pointer + 1]] : memory[instruction_pointer + 1]
      param2 = mode2 == 0 ? memory[memory[instruction_pointer + 2]] : memory[instruction_pointer + 2]
      memory[memory[instruction_pointer + 3]] = param1 + param2
      instruction_pointer += 4
    when 2
      param1 = mode1 == 0 ? memory[memory[instruction_pointer + 1]] : memory[instruction_pointer + 1]
      param2 = mode2 == 0 ? memory[memory[instruction_pointer + 2]] : memory[instruction_pointer + 2]
      memory[memory[instruction_pointer + 3]] = param1 * param2
      instruction_pointer += 4
    when 3
      memory[memory[instruction_pointer + 1]] = input[input_index]
      input_index += 1
      instruction_pointer += 2
    when 4
      output << (mode1 == 0 ? memory[memory[instruction_pointer + 1]] : memory[instruction_pointer + 1])
      instruction_pointer += 2
    when 5
      param1 = mode1 == 0 ? memory[memory[instruction_pointer + 1]] : memory[instruction_pointer + 1]
      param2 = mode2 == 0 ? memory[memory[instruction_pointer + 2]] : memory[instruction_pointer + 2]
      if param1 != 0
        instruction_pointer = param2
      else
        instruction_pointer += 3
      end
    when 6
      param1 = mode1 == 0 ? memory[memory[instruction_pointer + 1]] : memory[instruction_pointer + 1]
      param2 = mode2 == 0 ? memory[memory[instruction_pointer + 2]] : memory[instruction_pointer + 2]
      if param1 == 0
        instruction_pointer = param2
      else
        instruction_pointer += 3
      end
    when 7
      param1 = mode1 == 0 ? memory[memory[instruction_pointer + 1]] : memory[instruction_pointer + 1]
      param2 = mode2 == 0 ? memory[memory[instruction_pointer + 2]] : memory[instruction_pointer + 2]
      memory[memory[instruction_pointer + 3]] = param1 < param2 ? 1 : 0
      instruction_pointer += 4
    when 8
      param1 = mode1 == 0 ? memory[memory[instruction_pointer + 1]] : memory[instruction_pointer + 1]
      param2 = mode2 == 0 ? memory[memory[instruction_pointer + 2]] : memory[instruction_pointer + 2]
      memory[memory[instruction_pointer + 3]] = param1 == param2 ? 1 : 0
      instruction_pointer += 4
    when 99
      break
    else
      raise "Unknown opcode: #{opcode}"
    end
  end

  output
end

def permutations(items : Array(T)) : Array(Array(T)) forall T
  return [[] of T] if items.empty?

  result = [] of Array(T)
  items.each_with_index do |item, index|
    remaining = items[0...index] + items[(index + 1)..]
    permutations(remaining).each do |permutation|
      result << [item] + permutation
    end
  end
  result
end

def solve(program : Array(Int32)) : Int32
  max_signal = -1

  permutations([0, 1, 2, 3, 4]).each do |phase_settings|
    signal = 0
    phase_settings.each do |phase_setting|
      output = intcode(program, [phase_setting, signal])
      signal = output[0]
    end
    max_signal = signal if signal > max_signal
  end

  max_signal
end

program = File.read("input.txt").strip.split(",").map(&.to_i32)
puts solve(program)
