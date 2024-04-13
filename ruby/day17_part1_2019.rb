def decode(n)
  op = n % 100
  n /= 100
  modes = [n % 10, (n / 10) % 10, (n / 100) % 10]
  [op, modes]
end

class Machine
  attr_reader :data, :ip, :in, :out, :relbase

  def initialize(program, input, output)
    @data = Hash.new(0)
    program.each_with_index { |n, i| @data[i] = n }
    @ip = 0
    @relbase = 0
    @in = input
    @out = output
  end

  def get(i, mode)
    case mode
    when 1
      @data[i]
    when 0
      @data[@data[i]]
    when 2
      @data[@relbase + @data[i]]
    else
      raise "Unknown mode: #{mode}"
    end
  end

  def set(i, mode, val)
    case mode
    when 0
      @data[@data[i]] = val
    when 2
      @data[@relbase + @data[i]] = val
    else
      raise "Unknown mode: #{mode}"
    end
  end

  def step
    op, modes = decode(@data[@ip])
    case op
    when 1
      val = get(@ip+1, modes[0]) + get(@ip+2, modes[1])
      set(@ip+3, modes[2], val)
      @ip += 4
    when 2
      val = get(@ip+1, modes[0]) * get(@ip+2, modes[1])
      set(@ip+3, modes[2], val)
      @ip += 4
    when 3
      set(@ip+1, modes[0], @in.shift)
      @ip += 2
    when 4
      @out << get(@ip+1, modes[0])
      @ip += 2
    when 5
      @ip = get(@ip+1, modes[0]) != 0 ? get(@ip+2, modes[1]) : @ip + 3
    when 6
      @ip = get(@ip+1, modes[0]) == 0 ? get(@ip+2, modes[1]) : @ip + 3
    when 7
      set(@ip+3, modes[2], get(@ip+1, modes[0]) < get(@ip+2, modes[1]) ? 1 : 0)
      @ip += 4
    when 8
      set(@ip+3, modes[2], get(@ip+1, modes[0]) == get(@ip+2, modes[1]) ? 1 : 0)
      @ip += 4
    when 9
      @relbase += get(@ip+1, modes[0])
      @ip += 2
    when 99
      return false
    else
      raise "Unknown opcode: #{op}"
    end
    true
  end

  def run
    while step; end
  end
end

def run(program, input)
  output = []
  machine = Machine.new(program, input, output)
  machine.run
  output
end

def parse(program)
  out = run(program, [])
  scaffolding = {}
  robot = nil
  dir = nil
  out.pack('C*').split("\n").each_with_index do |line, y|
    line.chars.each_with_index do |ch, x|
      case ch
      when '^', 'v', '<', '>'
        robot = [x, y]
        dir = ch
        scaffolding[[x, y]] = true
      when '#'
        scaffolding[[x, y]] = true
      end
    end
  end
  [scaffolding, robot, dir]
end

def sum_align(grid)
  sum = 0
  grid.keys.each do |p|
    if [[0, 1], [0, -1], [1, 0], [-1, 0]].all? { |dx, dy| grid[[p[0] + dx, p[1] + dy]] }
      sum += p[0] * p[1]
    end
  end
  sum
end

program = File.read('input.txt').strip.split(',').map(&:to_i)
scaffolding, _, _ = parse(program)
puts sum_align(scaffolding)