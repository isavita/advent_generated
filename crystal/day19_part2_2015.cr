
#!/usr/bin/env crystal
# frozen_string_literal: true

def read_input(path : String) : String
  File.read(path).strip
end

def parse_input(data : String) : Tuple(Hash(String, String), Array(String), String)
  blocks = data.split("\n\n")
  # reverse graph: product => reactant
  rev = {} of String => String
  blocks[0].lines.each do |line|
    next if line.empty?
    left, right = line.split(" => ")
    rev[right] = left
  end
  # products sorted by length descending (greedy approach)
  products = rev.keys.sort_by { |s| -s.size }
  start_molecule = blocks[1].strip
  {rev, products, start_molecule}
end

def solve(data : String) : Int32
  rev, products, start = parse_input(data)

  mol = start
  steps = 0

  while mol != "e"
    changed = false
    products.each do |prod|
      if (idx = mol.index(prod))
        mol = mol.sub(prod, rev[prod])
        steps += 1
        changed = true
        break
      end
    end

    unless changed
      # fallback – reshuffle and restart (mirrors original random retry)
      mol = start
      steps = 0
      products.shuffle!
    end
  end

  steps
end

input = read_input("input.txt")
puts solve(input)
