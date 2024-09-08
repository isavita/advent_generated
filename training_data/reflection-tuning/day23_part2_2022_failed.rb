class ElfSimulation
  def initialize(input)
    @elves = Set.new
    @directions = [:N, :S, :W, :E]
    parse_input(input)
  end

  def parse_input(input)
    input.each_with_index do |line, y|
      line.chars.each_with_index do |char, x|
        @elves.add([x, y]) if char == '#'
      end
    end
  end

  def simulate(rounds)
    rounds.times do |round|
      proposed_moves = propose_moves
      break if proposed_moves.empty?
      move_elves(proposed_moves)
      rotate_directions
    end
    round + 1
  end

  def propose_moves
    proposed = Hash.new { |h, k| h[k] = [] }
    @elves.each do |elf|
      next if neighbors(elf).none? { |pos| @elves.include?(pos) }
      
      @directions.each do |dir|
        if can_move?(elf, dir)
          new_pos = move(elf, dir)
          proposed[new_pos] << elf
          break
        end
      end
    end
    proposed.select { |_, v| v.size == 1 }
  end

  def move_elves(proposed_moves)
    proposed_moves.each do |new_pos, old_pos|
      @elves.delete(old_pos.first)
      @elves.add(new_pos)
    end
  end

  def rotate_directions
    @directions.rotate!
  end

  def neighbors(pos)
    x, y = pos
    [
      [x-1, y-1], [x, y-1], [x+1, y-1],
      [x-1, y],             [x+1, y],
      [x-1, y+1], [x, y+1], [x+1, y+1]
    ]
  end

  def can_move?(elf, dir)
    case dir
    when :N then neighbors(elf)[0..2].none? { |pos| @elves.include?(pos) }
    when :S then neighbors(elf)[5..7].none? { |pos| @elves.include?(pos) }
    when :W then [neighbors(elf)[0], neighbors(elf)[3], neighbors(elf)[5]].none? { |pos| @elves.include?(pos) }
    when :E then [neighbors(elf)[2], neighbors(elf)[4], neighbors(elf)[7]].none? { |pos| @elves.include?(pos) }
    end
  end

  def move(pos, dir)
    x, y = pos
    case dir
    when :N then [x, y - 1]
    when :S then [x, y + 1]
    when :W then [x - 1, y]
    when :E then [x + 1, y]
    end
  end

  def count_empty_tiles
    min_x, max_x = @elves.map(&:first).minmax
    min_y, max_y = @elves.map(&:last).minmax
    (max_x - min_x + 1) * (max_y - min_y + 1) - @elves.size
  end
end

# Usage
input = File.readlines('input.txt').map(&:chomp)
simulation = ElfSimulation.new(input)
rounds = simulation.simulate(10)
puts "Part 1: #{simulation.count_empty_tiles}"

simulation = ElfSimulation.new(input)
rounds = simulation.simulate(Float::INFINITY)
puts "Part 2: #{rounds}"
