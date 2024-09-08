require 'set'

# Read and parse the input file
lines = File.readlines('input.txt', chomp: true)
molecule = lines.pop
lines.pop  # Remove empty line
replacements = {}

lines.each do |line|
  from, to = line.split(' => ')
  replacements[from] ||= []
  replacements[from] << to
end

# Function to generate all possible molecules after one replacement
def generate_molecules(molecule, replacements)
  new_molecules = Set.new

  replacements.each do |from, to_list|
    start_index = 0
    while (index = molecule.index(from, start_index))
      to_list.each do |to|
        new_molecule = molecule.dup
        new_molecule[index, from.length] = to
        new_molecules.add(new_molecule)
      end
      start_index = index + 1
    end
  end

  new_molecules
end

# Generate all possible molecules and count them
result = generate_molecules(molecule, replacements).size

puts result
