def parse_reactions(file_path)
  reactions = {}
  File.readlines(file_path).each do |line|
    inputs, output = line.strip.split(' => ')
    out_qty, out_chem = output.split
    reactions[out_chem] = {
      quantity: out_qty.to_i,
      inputs: inputs.split(', ').map { |i| i.split.reverse }.to_h { |c, q| [c, q.to_i] }
    }
  end
  reactions
end

def calculate_ore(reactions, chemical, quantity, excess = {})
  return quantity if chemical == 'ORE'

  excess[chemical] ||= 0
  if excess[chemical] >= quantity
    excess[chemical] -= quantity
    return 0
  end

  quantity -= excess[chemical]
  excess[chemical] = 0

  reaction = reactions[chemical]
  times = (quantity.to_f / reaction[:quantity]).ceil
  ore_needed = 0

  reaction[:inputs].each do |input_chem, input_qty|
    ore_needed += calculate_ore(reactions, input_chem, input_qty * times, excess)
  end

  excess[chemical] += times * reaction[:quantity] - quantity
  ore_needed
end

reactions = parse_reactions('input.txt')
ore_required = calculate_ore(reactions, 'FUEL', 1)
puts ore_required
