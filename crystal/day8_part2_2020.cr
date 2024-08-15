require "file_utils"

def execute_boot_code(instructions)
  accumulator = 0
  visited = Set(Int32).new
  current_instruction = 0

  while current_instruction < instructions.size
    return accumulator, false if visited.includes? current_instruction

    visited << current_instruction
    op, arg = parse_instruction(instructions[current_instruction])

    case op
    when "acc"
      accumulator += arg
      current_instruction += 1
    when "jmp"
      current_instruction += arg
    when "nop"
      current_instruction += 1
    end
  end

  {accumulator, true}
end

def parse_instruction(instruction)
  parts = instruction.split
  {parts[0], parts[1].to_i}
end

instructions = File.read("input.txt").split("\n")

instructions.each_with_index do |instruction, i|
  op, arg = parse_instruction(instruction)
  next if op == "acc"

  modified_instructions = instructions.dup
  modified_instructions[i] = if op == "jmp"
                                 "nop #{arg}"
                               else
                                 "jmp #{arg}"
                               end

  accumulator, terminated = execute_boot_code(modified_instructions)
  puts accumulator if terminated
end