
class IntcodeComputer
  getter memory : Hash(Int64, Int64)
  getter ip : Int64
  getter relative_base : Int64
  getter input_queue : Deque(Int64)
  getter output_queue : Deque(Int64)
  getter halted : Bool
  getter waiting_for_input : Bool

  def initialize(program : Array(Int64))
    @memory = Hash(Int64, Int64).new(0)
    program.each_with_index { |v, i| @memory[i.to_i64] = v }
    @ip = 0_i64
    @relative_base = 0_i64
    @input_queue = Deque(Int64).new
    @output_queue = Deque(Int64).new
    @halted = false
    @waiting_for_input = false
  end

  def add_input(val : Int64)
    input_queue << val
    @waiting_for_input = false
  end

  def run
    return if halted

    while true
      instruction = memory[ip]
      opcode = instruction % 100
      modes = [
        (instruction // 100) % 10,
        (instruction // 1000) % 10,
        (instruction // 10000) % 10
      ]

      break if opcode == 99

      get_param = ->(offset : Int32, mode : Int64) do
        val_at_offset = memory[ip + offset]
        case mode
        when 0 then memory[val_at_offset]
        when 1 then val_at_offset
        when 2 then memory[relative_base + val_at_offset]
        else        0_i64
        end
      end

      get_write_addr = ->(offset : Int32, mode : Int64) do
        val_at_offset = memory[ip + offset]
        case mode
        when 0 then val_at_offset
        when 2 then relative_base + val_at_offset
        else        0_i64
        end
      end

      case opcode
      when 1
        val1 = get_param.call(1, modes[0])
        val2 = get_param.call(2, modes[1])
        addr3 = get_write_addr.call(3, modes[2])
        memory[addr3] = val1 + val2
        @ip += 4
      when 2
        val1 = get_param.call(1, modes[0])
        val2 = get_param.call(2, modes[1])
        addr3 = get_write_addr.call(3, modes[2])
        memory[addr3] = val1 * val2
        @ip += 4
      when 3
        if input_queue.empty?
          @waiting_for_input = true
          return
        end
        addr1 = get_write_addr.call(1, modes[0])
        memory[addr1] = input_queue.shift
        @ip += 2
      when 4
        val1 = get_param.call(1, modes[0])
        output_queue << val1
        @ip += 2
        return
      when 5
        val1 = get_param.call(1, modes[0])
        val2 = get_param.call(2, modes[1])
        @ip = val1 != 0 ? val2 : ip + 3
      when 6
        val1 = get_param.call(1, modes[0])
        val2 = get_param.call(2, modes[1])
        @ip = val1 == 0 ? val2 : ip + 3
      when 7
        val1 = get_param.call(1, modes[0])
        val2 = get_param.call(2, modes[1])
        addr3 = get_write_addr.call(3, modes[2])
        memory[addr3] = val1 < val2 ? 1_i64 : 0_i64
        @ip += 4
      when 8
        val1 = get_param.call(1, modes[0])
        val2 = get_param.call(2, modes[1])
        addr3 = get_write_addr.call(3, modes[2])
        memory[addr3] = val1 == val2 ? 1_i64 : 0_i64
        @ip += 4
      when 9
        val1 = get_param.call(1, modes[0])
        @relative_base += val1
        @ip += 2
      else
        @halted = true
        break
      end
    end
    @halted = true
  end

  def has_output? : Bool
    !output_queue.empty?
  end

  def get_output : Int64
    output_queue.shift
  end
end

def parse_input(file_path : String) : Array(Int64)
  File.read(file_path).chomp.split(',').map(&.to_i64)
end

def play_game(program_initial : Array(Int64)) : Int64
  computer = IntcodeComputer.new(program_initial.dup)
  computer.memory[0] = 2_i64

  score = 0_i64
  ball_x = 0_i64
  paddle_x = 0_i64

  until computer.halted
    computer.run

    break if computer.halted

    if computer.waiting_for_input
      input_val = ball_x > paddle_x ? 1_i64 : (ball_x < paddle_x ? -1_i64 : 0_i64)
      computer.add_input(input_val)
      next
    end

    if computer.has_output?
      x = computer.get_output
      computer.run
      y = computer.get_output
      computer.run
      tile_id = computer.get_output

      if x == -1 && y == 0
        score = tile_id
      else
        paddle_x = x if tile_id == 3
        ball_x = x if tile_id == 4
      end
    end
  end
  score
end

program = parse_input("input.txt")
final_score = play_game(program)
puts final_score
