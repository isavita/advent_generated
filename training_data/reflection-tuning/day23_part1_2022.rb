require 'set'

def parse_input(filename)
  elves = Set.new
  File.readlines(filename).each_with_index do |line, y|
    line.chomp.chars.each_with_index do |char, x|
      elves.add([x, y]) if char == '#'
    end
  end
  elves
end

def adjacent_positions(x, y)
  [
    [x-1, y-1], [x, y-1], [x+1, y-1],
    [x-1, y  ],           [x+1, y  ],
    [x-1, y+1], [x, y+1], [x+1, y+1]
  ]
end

def propose_move(elf, elves, round)
  x, y = elf
  return nil if adjacent_positions(x, y).none? { |pos| elves.include?(pos) }

  directions = [
    [[x, y-1], [x-1, y-1], [x+1, y-1]],  # N
    [[x, y+1], [x-1, y+1], [x+1, y+1]],  # S
    [[x-1, y], [x-1, y-1], [x-1, y+1]],  # W
    [[x+1, y], [x+1, y-1], [x+1, y+1]]   # E
  ]
  directions.rotate!(round % 4)

  directions.each do |dir|
    return dir[0] if dir.none? { |pos| elves.include?(pos) }
  end
  nil
end

def simulate_round(elves, round)
  proposals = {}
  proposal_counts = Hash.new(0)

  elves.each do |elf|
    proposal = propose_move(elf, elves, round)
    if proposal
      proposals[elf] = proposal
      proposal_counts[proposal] += 1
    end
  end

  moved = false
  proposals.each do |elf, proposal|
    if proposal_counts[proposal] == 1
      elves.delete(elf)
      elves.add(proposal)
      moved = true
    end
  end

  moved
end

def find_boundaries(elves)
  min_x, max_x = elves.map(&:first).minmax
  min_y, max_y = elves.map(&:last).minmax
  [min_x, max_x, min_y, max_y]
end

def count_empty_tiles(elves)
  min_x, max_x, min_y, max_y = find_boundaries(elves)
  (max_x - min_x + 1) * (max_y - min_y + 1) - elves.size
end

elves = parse_input('input.txt')

10.times do |round|
  simulate_round(elves, round)
end

puts count_empty_tiles(elves)
