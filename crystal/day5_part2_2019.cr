
program = File.read("input.txt").strip.split(",").map(&.to_i)
input = 5
output = 0
i = 0

def get_value(program : Array(Int32), pos : Int32, mode : Int32) : Int32
  mode == 0 ? program[program[pos]] : program[pos]
end

while true
  opcode = program[i] % 100
  modes = program[i] // 100
  param1_mode = modes % 10
  modes //= 10
  param2_mode = modes % 10

  case opcode
  when 1
    p1 = get_value(program, i + 1, param1_mode)
    p2 = get_value(program, i + 2, param2_mode)
    p3 = program[i + 3]
    program[p3] = p1 + p2
    i += 4
  when 2
    p1 = get_value(program, i + 1, param1_mode)
    p2 = get_value(program, i + 2, param2_mode)
    p3 = program[i + 3]
    program[p3] = p1 * p2
    i += 4
  when 3
    program[program[i + 1]] = input
    i += 2
  when 4
    output = get_value(program, i + 1, param1_mode)
    puts output
    i += 2
  when 5
    p1 = get_value(program, i + 1, param1_mode)
    p2 = get_value(program, i + 2, param2_mode)
    i = p1 != 0 ? p2 : i + 3
  when 6
    p1 = get_value(program, i + 1, param1_mode)
    p2 = get_value(program, i + 2, param2_mode)
    i = p1 == 0 ? p2 : i + 3
  when 7
    p1 = get_value(program, i + 1, param1_mode)
    p2 = get_value(program, i + 2, param2_mode)
    p3 = program[i + 3]
    program[p3] = p1 < p2 ? 1 : 0
    i += 4
  when 8
    p1 = get_value(program, i + 1, param1_mode)
    p2 = get_value(program, i + 2, param2_mode)
    p3 = program[i + 3]
    program[p3] = p1 == p2 ? 1 : 0
    i += 4
  when 99
    break
  else
    raise "Invalid opcode"
  end
end
