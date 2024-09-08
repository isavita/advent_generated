def parse_input(filename)
  replacements = []
  medicine = ''
  File.readlines(filename).each do |line|
    if line.include?(' => ')
      from, to = line.strip.split(' => ')
      replacements << [from, to]
    elsif line.strip != ''
      medicine = line.strip
    end
  end
  [replacements, medicine]
end

def count_distinct_molecules(replacements, medicine)
  molecules = Set.new
  replacements.each do |from, to|
    i = -1
    while (i = medicine.index(from, i + 1))
      new_molecule = medicine.dup
      new_molecule[i, from.length] = to
      molecules.add(new_molecule)
    end
  end
  molecules.size
end

def steps_to_medicine(replacements, target)
  steps = 0
  current = target
  while current != 'e'
    old = current
    replacements.shuffle.each do |from, to|
      while current.include?(to)
        current = current.sub(to, from)
        steps += 1
      end
    end
    return steps_to_medicine(replacements, target) if old == current
  end
  steps
end

replacements, medicine = parse_input('input.txt')

# Part One
puts "Part One: #{count_distinct_molecules(replacements, medicine)}"

# Part Two
puts "Part Two: #{steps_to_medicine(replacements, medicine)}"
