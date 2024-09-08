def parse_rules(input)
  rules = {}
  input.each_line do |line|
    from, to = line.strip.split(' => ')
    rules[from] = to
  end
  rules
end

def split_grid(grid)
  size = grid.size
  step = size.even? ? 2 : 3
  (0...size).step(step).map do |i|
    (0...size).step(step).map do |j|
      grid[i,step].map { |row| row[j,step] }
    end
  end
end

def rotate(square)
  square.transpose.map(&:reverse)
end

def flip(square)
  square.map(&:reverse)
end

def variations(square)
  rotations = 4.times.map { |i| i.times.inject(square) { |s| rotate(s) } }
  rotations + rotations.map { |r| flip(r) }
end

def enhance_square(square, rules)
  key = square.map(&:join).join('/')
  variations(square).each do |variation|
    var_key = variation.map(&:join).join('/')
    return rules[var_key].split('/').map(&:chars) if rules.key?(var_key)
  end
  raise "No matching rule found for #{key}"
end

def enhance_grid(grid, rules)
  split_grid(grid).map do |row|
    row.map { |square| enhance_square(square, rules) }
  end.flat_map do |row|
    row.transpose.map(&:flatten)
  end
end

def count_on_pixels(grid)
  grid.sum { |row| row.count('#') }
end

def solve(input, iterations)
  rules = parse_rules(input)
  grid = ['.#.'.chars, '..#'.chars, '###'.chars]

  iterations.times do
    grid = enhance_grid(grid, rules)
  end

  count_on_pixels(grid)
end

input = File.read('input.txt')
puts solve(input, 5)  # Part 1
puts solve(input, 18) # Part 2
