
class Machine
  def initialize(program, input, output)
    @data = Hash.new(0)
    program.each_with_index { |n, i| @data[i] = n }
    @ip = 0
    @input = input
    @output = output
    @relbase = 0
  end

  def get(i, mode)
    case mode
    when 0
      @data[@data[i]]
    when 1
      @data[i]
    when 2
      @data[@relbase + @data[i]]
    end
  end

  def set(i, mode, val)
    case mode
    when 0
      @data[@data[i]] = val
    when 2
      @data[@relbase + @data[i]] = val
    end
  end

  def step
    op, modes = decode(@data[@ip])
    case op
    when 1
      val = get(@ip + 1, modes[0]) + get(@ip + 2, modes[1])
      set(@ip + 3, modes[2], val)
      @ip += 4
    when 2
      val = get(@ip + 1, modes[0]) * get(@ip + 2, modes[1])
      set(@ip + 3, modes[2], val)
      @ip += 4
    when 3
      set(@ip + 1, modes[0], @input.shift)
      @ip += 2
    when 4
      @output << get(@ip + 1, modes[0])
      @ip += 2
    when 5
      if get(@ip + 1, modes[0]) != 0
        @ip = get(@ip + 2, modes[1])
      else
        @ip += 3
      end
    when 6
      if get(@ip + 1, modes[0]) == 0
        @ip = get(@ip + 2, modes[1])
      else
        @ip += 3
      end
    when 7
      if get(@ip + 1, modes[0]) < get(@ip + 2, modes[1])
        set(@ip + 3, modes[2], 1)
      else
        set(@ip + 3, modes[2], 0)
      end
      @ip += 4
    when 8
      if get(@ip + 1, modes[0]) == get(@ip + 2, modes[1])
        set(@ip + 3, modes[2], 1)
      else
        set(@ip + 3, modes[2], 0)
      end
      @ip += 4
    when 9
      @relbase += get(@ip + 1, modes[0])
      @ip += 2
    when 99
      return false
    end
    true
  end

  def run
    step while step
  end

  def decode(n)
    op = n % 100
    n /= 100
    modes = []
    3.times do
      modes << n % 10
      n /= 10
    end
    [op, modes]
  end
end

def manhattan(p, q)
  (p[0] - q[0]).abs + (p[1] - q[1]).abs
end

class PathFinder
  NEIGHBORS = [[0, 1], [0, -1], [1, 0], [-1, 0]]
  DIR_MAP = { [0, 1] => 1, [0, -1] => 2, [-1, 0] => 3, [1, 0] => 4 }

  def initialize(program)
    @grid = Hash.new
    @input = []
    @output = []
    @grid[[0, 0]] = '.'
    @m = Machine.new(program, @input, @output)
    @p = [0, 0]
    @oxygen = nil
    Thread.new { @m.run }
  end

  def try_move(dir)
    @input << DIR_MAP[dir]
    next_p = [@p[0] + dir[0], @p[1] + dir[1]]
    case @output.shift
    when 0
      @grid[next_p] = '#'
      return false
    when 1
      @grid[next_p] = '.'
    when 2
      @grid[next_p] = 'O'
      @oxygen = next_p
    end
    @p = next_p
    true
  end

  def open
    ps = {}
    @grid.each do |p, b|
      next unless b == '#'
      NEIGHBORS.each do |n|
        pn = [p[0] + n[0], p[1] + n[1]]
        if !@grid.key?(pn)
          ps[p] = true
          break
        end
      end
    end
    ps
  end
  
  def explore
    while open.any?
      if !open.key?(@p)
        min_dist = Float::INFINITY
        next_p = nil
        open.keys.each do |to|
          dist = manhattan(@p, to)
          if dist < min_dist
            min_dist = dist
            next_p = to
          end
        end
        min_path = shortest_path(@p, next_p)
        min_path.each do |move|
          raise 'bad path' unless try_move(move)
        end
      end
      loop do
        moved = false
        NEIGHBORS.shuffle.each do |n|
          pn = [@p[0] + n[0], @p[1] + n[1]]
          if !@grid.key?(pn)
            moved = try_move(n)
            break
          end
        end
        break unless moved
      end
    end
  end

  def shortest_path(from, to)
    pq = [[from, 0]]
    path_map = { from => [] }
    until pq.empty?
      curr, _ = pq.pop
      curr_path = path_map[curr]
      break if curr == to
      NEIGHBORS.each do |n|
        next_p = [curr[0] + n[0], curr[1] + n[1]]
        next if !@grid.key?(next_p) || @grid[next_p] == '#'
        if !path_map.key?(next_p) || path_map[next_p].length > 1 + curr_path.length
          next_path = curr_path + [n]
          pq.push([next_p, -next_path.length])
          pq.sort_by! { |_, priority| priority }
          path_map[next_p] = next_path
        end
      end
    end
    path_map[to]
  end

  def longest_path(from)
    pq = [[from, 0]]
    dist_map = { from => 0 }
    until pq.empty?
      curr, _ = pq.pop
      curr_dist = dist_map[curr]
      NEIGHBORS.each do |n|
        next_p = [curr[0] + n[0], curr[1] + n[1]]
        next if !@grid.key?(next_p) || @grid[next_p] == '#'
        if !dist_map.key?(next_p) || dist_map[next_p] > 1 + curr_dist
          next_dist = 1 + curr_dist
          pq.push([next_p, -next_dist])
          pq.sort_by! { |_, priority| priority }
          dist_map[next_p] = next_dist
        end
      end
    end
    dist_map.values.max
  end
end

program = File.read('input.txt').strip.split(',').map(&:to_i)
pf = PathFinder.new(program)
pf.explore
puts pf.longest_path(pf.oxygen)
