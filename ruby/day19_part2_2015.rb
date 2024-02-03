
file = File.read("input.txt")
input = file.strip

def parse_input(input)
  blocks = input.split("\n\n")
  starting_material = split_molecules(blocks[1])

  graph = {}

  blocks[0].split("\n").each do |l|
    parts = l.split(" => ")
    graph[parts[0]] ||= []
    graph[parts[0]] << parts[1]
  end

  return graph, starting_material
end

def split_molecules(input)
  molecules = []
  input.each_char do |char|
    code = char.ord
    if code >= 65 && code <= 90
      molecules << char
    else
      molecules[-1] += char
    end
  end
  return molecules
end

def solve(input)
  reverse_graph, starting_mols = parse_input(input)

  product_to_reactant = {}
  reverse_graph.each do |react, products|
    products.each do |p|
      if product_to_reactant.key?(p)
        raise "dup found"
      end
      product_to_reactant[p] = react
    end
  end

  all_products = product_to_reactant.keys

  start = starting_mols.join("")
  mol = start

  steps = 0
  until mol == "e"
    change_made = false
    all_products.each do |prod|
      count = mol.scan(/#{prod}/).length
      if count <= 0
        next
      end
      change_made = true
      steps += count
      mol = mol.gsub(prod, product_to_reactant[prod])

      break
    end

    if !change_made
      all_products.shuffle!
      mol = start
      steps = 0
    end
  end

  return steps
end

def shuffle_slice(in_arr)
  in_arr.shuffle
end

puts solve(input)
