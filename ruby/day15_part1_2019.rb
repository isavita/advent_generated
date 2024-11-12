require 'matrix'

class Machine
  OPCODES = {
    1 => :add, 2 => :mul, 3 => :input, 4 => :output,
    5 => :jt, 6 => :jf, 7 => :lt, 8 => :eq,
    9 => :rbo, 99 => :halt
  }.freeze

  MODES = {
    0 => :position, 1 => :immediate, 2 => :relative
  }.freeze

  attr_accessor :data, :ip, :relbase

  def initialize(program, input, output)
    @data = program.each_with_index.to_h
    @ip = 0
    @relbase = 0
    @input = input
    @output = output
  end

  def get(i, mode)
    case MODES[mode]
    when :immediate
      @data[i] || 0
    when :position
      @data[@data[i] || 0] || 0
    when :relative
      @data[@relbase + (@data[i] || 0)] || 0
    end
  end

  def set(i, mode, value)
    case MODES[mode]
    when :position
      @data[@data[i] || 0] = value
    when :relative
      @data[@relbase + (@data[i] || 0)] = value
    end
  end

  def step
    opcode, modes = decode(@data[@ip])
    send(OPCODES[opcode], modes)
  end

  def run
    while step; end
    @output.close
  end

  private

  def decode(n)
    opcode = n % 100
    modes = []
    3.times { modes << (n /= 10) % 10 }
    [opcode, modes]
  end

  def add(modes)
    set(@ip + 3, modes[2], get(@ip + 1, modes[0]) + get(@ip + 2, modes[1]))
    @ip += 4
  end

  def mul(modes)
    set(@ip + 3, modes[2], get(@ip + 1, modes[0]) * get(@ip + 2, modes[1]))
    @ip += 4
  end

  def input(modes)
    set(@ip + 1, modes[0], @input.pop)
    @ip += 2
  end

  def output(modes)
    @output << get(@ip + 1, modes[0])
    @ip += 2
  end

  def jt(modes)
    if get(@ip + 1, modes[0]) != 0
      @ip = get(@ip + 2, modes[1])
    else
      @ip += 3
    end
  end

  def jf(modes)
    if get(@ip + 1, modes[0]) == 0
      @ip = get(@ip + 2, modes[1])
    else
      @ip += 3
    end
  end

  def lt(modes)
    set(@ip + 3, modes[2], get(@ip + 1, modes[0]) < get(@ip + 2, modes[1]) ? 1 : 0)
    @ip += 4
  end

  def eq(modes)
    set(@ip + 3, modes[2], get(@ip + 1, modes[0]) == get(@ip + 2, modes[1]) ? 1 : 0)
    @ip += 4
  end

  def rbo(modes)
    @relbase += get(@ip + 1, modes[0])
    @ip += 2
  end

  def halt(_modes)
    false
  end
end

class Pathfinder
  DIRECTIONS = {
    N: [0, 1], S: [0, -1], W: [-1, 0], E: [1, 0]
  }.freeze

  DIRECTION_MAP = {
    N: 1, S: 2, W: 3, E: 4
  }.freeze

  NEIGHBORS = DIRECTIONS.values

  def initialize(program)
    @grid = Hash.new('.')
    @input = []
    @output = []
    @machine = Machine.new(program, @input, @output)
    @position = [0, 0]
    @oxygen = nil
  end

  def explore
    Thread.new { @machine.run }
    while !@output.empty? || !@input.empty?
      move_to_unexplored
      explore_neighbors
    end
  end

  def shortest_path(from, to)
    pq = [[0, from]]
    distmap = { from => 0 }
    until pq.empty?
      dist, curr = pq.shift
      next if dist > distmap[curr]

      NEIGHBORS.each do |dx, dy|
        next_pos = [curr[0] + dx, curr[1] + dy]
        next if @grid[next_pos] == '#'

        new_dist = dist + 1
        if !distmap[next_pos] || new_dist < distmap[next_pos]
          distmap[next_pos] = new_dist
          pq << [new_dist, next_pos]
          pq.sort_by!(&:first)
        end
      end
    end
    distmap[to]
  end

  private

  def move_to_unexplored
    unexplored = @grid.select { |_, v| v == '.' }
    return if unexplored.empty?

    closest = unexplored.keys.min_by { |pos| manhattan_distance(@position, pos) }
    path = shortest_path(@position, closest)

    path.each do |dx, dy|
      dir = DIRECTIONS.key([dx, dy])
      @input.unshift(DIRECTION_MAP[dir])
      @machine.step until @output.any?
      case @output.pop
      when 0
        @grid[[@position[0] + dx, @position[1] + dy]] = '#'
        return
      when 1, 2
        @position = [@position[0] + dx, @position[1] + dy]
        @grid[@position] = @output.last == 2 ? 'O' : '.'
        @oxygen = @position if @output.last == 2
      end
    end
  end

  def explore_neighbors
    NEIGHBORS.each do |dx, dy|
      next_pos = [@position[0] + dx, @position[1] + dy]
      next if @grid[next_pos] != '.'

      dir = DIRECTIONS.key([dx, dy])
      @input.unshift(DIRECTION_MAP[dir])
      @machine.step until @output.any?
      case @output.pop
      when 0
        @grid[next_pos] = '#'
      when 1, 2
        @position = next_pos
        @grid[@position] = @output.last == 2 ? 'O' : '.'
        @oxygen = @position if @output.last == 2
        return
      end
    end
  end

  def manhattan_distance(a, b)
    (a[0] - b[0]).abs + (a[1] - b[1]).abs
  end
end

program = File.read('input.txt').split(',').map(&:to_i)
pf = Pathfinder.new(program)
pf.explore
puts pf.shortest_path([0, 0], pf.oxygen)