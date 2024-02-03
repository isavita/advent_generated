
def run_intcode(program)
  pc = 0
  relative_base = 0
  output = []
  score = 0
  ball_x = paddle_x = 0

  read_input = -> { (ball_x <=> paddle_x) }

  while program[pc] != 99
    opcode, modes = program[pc] % 100, program[pc] / 100
    param = ->(i) do
      mode = modes / (10 ** (i - 1)) % 10
      val = program[pc + i]
      mode == 1 ? val : program[mode == 2 ? relative_base + val : val] || 0
    end
    write = ->(i, value) do
      mode = modes / (10 ** (i - 1)) % 10
      addr = mode == 2 ? relative_base + program[pc + i] : program[pc + i]
      program[addr] = value
    end

    case opcode
    when 1
      write[3, param[1] + param[2]]
      pc += 4
    when 2
      write[3, param[1] * param[2]]
      pc += 4
    when 3
      write[1, read_input.call]
      pc += 2
    when 4
      output << param[1]
      if output.size == 3
        if output[0] == -1 && output[1] == 0
          score = output[2]
        else
          ball_x = output[0] if output[2] == 4
          paddle_x = output[0] if output[2] == 3
        end
        output.clear
      end
      pc += 2
    when 5
      pc = param[1] != 0 ? param[2] : pc + 3
    when 6
      pc = param[1] == 0 ? param[2] : pc + 3
    when 7
      write[3, param[1] < param[2] ? 1 : 0]
      pc += 4
    when 8
      write[3, param[1] == param[2] ? 1 : 0]
      pc += 4
    when 9
      relative_base += param[1]
      pc += 2
    else
      raise "Unknown opcode #{opcode}"
    end
  end

  score
end

program = File.read("input.txt").split(",").map(&:to_i)
program[0] = 2
puts run_intcode(program)
