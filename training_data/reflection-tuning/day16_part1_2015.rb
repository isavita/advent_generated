# Read the input file
aunts = File.readlines('input.txt').map do |line|
  name, attrs = line.strip.split(': ', 2)
  [name.split(' ')[1].to_i, attrs.split(', ').map { |attr| attr.split(': ') }.to_h]
end

# MFCSAM readings
mfcsam = {
  'children' => 3, 'cats' => 7, 'samoyeds' => 2, 'pomeranians' => 3,
  'akitas' => 0, 'vizslas' => 0, 'goldfish' => 5, 'trees' => 3,
  'cars' => 2, 'perfumes' => 1
}

# Find the matching Aunt Sue
result = aunts.find do |number, attributes|
  attributes.all? do |key, value|
    mfcsam[key] == value.to_i
  end
end

puts result[0]
