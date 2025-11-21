
require "json"

class IntcodeComputer
  property memory : Hash(Int64, Int64)
  property ip : Int64 = 0
  property relative_base : Int64 = 0
  property halted : Bool = false
  property input_queue : Array(Int64) = [] of Int64
  property output_queue : Array(Int64) = [] of Int64

  def initialize(program : Array(Int64))
    @memory = {} of Int64 => Int64
    program.each_with_index { |v, i| @memory[i.to_i64] = v }
  end

  def fetch(addr : Int64) : Int64
    @memory.fetch(addr, 0_i64)
  end

  def write(addr : Int64, value : Int64)
    @memory[addr] = value
  end

  def get_parameter(mode : Int32, offset : Int32) : Int64
    param = fetch(@ip + offset)
    case mode
    when 0 then fetch(param)
    when 1 then param
    when 2 then fetch(@relative_base + param)
    else    raise "bad mode #{mode}"
    end
  end

  def set_parameter(mode : Int32, offset : Int32, value : Int64)
    target = fetch(@ip + offset)
    case mode
    when 0 then write(target, value)
    when 2 then write(@relative_base + target, value)
    else    raise "bad write mode #{mode}"
    end
  end

  def run
    while !@halted
      instr = fetch(@ip)
      opcode = (instr % 100).to_i32
      modes = [(instr / 100 % 10).to_i32, (instr / 1000 % 10).to_i32, (instr / 10000 % 10).to_i32]

      case opcode
      when 1
        a = get_parameter(modes[0], 1)
        b = get_parameter(modes[1], 2)
        set_parameter(modes[2], 3, a + b)
        @ip += 4
      when 2
        a = get_parameter(modes[0], 1)
        b = get_parameter(modes[1], 2)
        set_parameter(modes[2], 3, a * b)
        @ip += 4
      when 3
        return if @input_queue.empty?
        set_parameter(modes[0], 1, @input_queue.shift)
        @ip += 2
      when 4
        @output_queue << get_parameter(modes[0], 1)
        @ip += 2
        return
      when 5
        if get_parameter(modes[0], 1) != 0
          @ip = get_parameter(modes[1], 2)
        else
          @ip += 3
        end
      when 6
        if get_parameter(modes[0], 1) == 0
          @ip = get_parameter(modes[1], 2)
        else
          @ip += 3
        end
      when 7
        set_parameter(modes[2], 3, (get_parameter(modes[0], 1) < get_parameter(modes[1], 2)) ? 1_i64 : 0_i64)
        @ip += 4
      when 8
        set_parameter(modes[2], 3, (get_parameter(modes[0], 1) == get_parameter(modes[1], 2)) ? 1_i64 : 0_i64)
        @ip += 4
      when 9
        @relative_base += get_parameter(modes[0], 1)
        @ip += 2
      when 99
        @halted = true
        break
      else
        raise "unknown opcode #{opcode}"
      end
    end
  end
end

class Droid
  DIR = {1 => {0, -1}, 2 => {0, 1}, 3 => {-1, 0}, 4 => {1, 0}}
  getter computer : IntcodeComputer
  getter grid : Hash({Int32, Int32}, Int32) = {} of {Int32, Int32} => Int32
  getter current_position : {Int32, Int32} = {0, 0}
  getter oxygen_position : {Int32, Int32} = {0, 0}
  getter oxygen_found : Bool = false

  def initialize(program : Array(Int64))
    @computer = IntcodeComputer.new(program)
    @grid[@current_position] = 1
  end

  def send_move_command(dir : Int32) : Int32
    @computer.input_queue << dir
    while @computer.output_queue.empty? && !@computer.halted
      @computer.run
    end
    raise "halted without output" if @computer.halted && @computer.output_queue.empty?
    @computer.output_queue.shift.to_i32
  end

  def opposite(dir : Int32) : Int32
    {1 => 2, 2 => 1, 3 => 4, 4 => 3}[dir]
  end

  def find_path(start : {Int32, Int32}, goal : {Int32, Int32}) : Array(Int32)
    q = [{start, [] of Int32}]
    visited = Set({Int32, Int32}).new
    visited << start
    until q.empty?
      pos, path = q.shift
      return path if pos == goal
      DIR.each_key do |d|
        dx, dy = DIR[d]
        nxt = {pos[0] + dx, pos[1] + dy}
        next if visited.includes?(nxt)
        case @grid[nxt]?
        when nil, 0 then next
        else
          visited << nxt
          q << {nxt, path + [d]}
        end
      end
    end
    raise "no path"
  end

  def move_to(target : {Int32, Int32})
    return if @current_position == target
    find_path(@current_position, target).each do |d|
      status = send_move_command(d)
      raise "wall on known path" if status == 0
      dx, dy = DIR[d]
      @current_position = {@current_position[0] + dx, @current_position[1] + dy}
      if status == 2
        @oxygen_position = @current_position
        @oxygen_found = true
      end
    end
  end

  def explore : Int32
    q = [{@current_position, 0}]
    visited = Set({Int32, Int32}).new
    visited << @current_position
    until q.empty?
      pos, steps = q.shift
      move_to(pos)
      DIR.each_key do |d|
        dx, dy = DIR[d]
        nxt = {pos[0] + dx, pos[1] + dy}
        next if visited.includes?(nxt)
        status = send_move_command(d)
        if status == 0
          @grid[nxt] = 0
        else
          @grid[nxt] = (status == 1) ? 1 : 2
          visited << nxt
          q << {nxt, steps + 1}
          if status == 2
            @oxygen_position = nxt
            @oxygen_found = true
            return steps + 1
          end
          send_move_command(opposite(d))
        end
      end
    end
    -1
  end
end

def parse_input(path : String) : Array(Int64)
  File.read(path).strip.split(',').map(&.to_i64)
end

program = parse_input("input.txt")
droid = Droid.new(program)
puts droid.explore
