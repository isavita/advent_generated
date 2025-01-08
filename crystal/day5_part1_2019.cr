
def get_mode(instruction : Int32, position : Int32) : Int32
  instruction // (10_i32 ** (position + 1)) % 10
end

def get_param(program : Array(Int32), pointer : Int32, mode : Int32) : Int32
  if mode == 0
    program[program[pointer]]
  else
    program[pointer]
  end
end

def run_program(program : Array(Int32), input : Int32) : Int32
  output = 0
  pointer = 0
  while pointer < program.size
    instruction = program[pointer]
    opcode = instruction % 100

    case opcode
    when 1, 2
      param1 = get_param(program, pointer + 1, get_mode(instruction, 1))
      param2 = get_param(program, pointer + 2, get_mode(instruction, 2))
      result = opcode == 1 ? param1 + param2 : param1 * param2
      program[program[pointer + 3]] = result
      pointer += 4
    when 3
      program[program[pointer + 1]] = input
      pointer += 2
    when 4
      output = get_param(program, pointer + 1, get_mode(instruction, 1))
      pointer += 2
    when 99
      return output
    else
      raise "Unknown opcode: #{opcode}"
    end
  end
  output
end

data = File.read("input.txt").strip.split(",").map(&.to_i32)
puts run_program(data, 1)
