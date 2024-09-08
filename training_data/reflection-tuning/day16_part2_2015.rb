# Read and parse input
aunts = File.readlines('input.txt').map do |line|
  name, attrs = line.strip.split(': ', 2)
  attrs = attrs.split(', ').map { |attr| attr.split(': ') }.to_h
  [name.split[1].to_i, attrs]
end.to_h

# MFCSAM readings
mfcsam = {
  'children' => 3, 'cats' => 7, 'samoyeds' => 2, 'pomeranians' => 3,
  'akitas' => 0, 'vizslas' => 0, 'goldfish' => 5, 'trees' => 3,
  'cars' => 2, 'perfumes' => 1
}

# Part 1: Exact match
part1 = aunts.find do |_, attrs|
  attrs.all? { |k, v| mfcsam[k] == v.to_i }
end

# Part 2: Range match
part2 = aunts.find do |_, attrs|
  attrs.all? do |k, v|
    case k
    when 'cats', 'trees'
      v.to_i > mfcsam[k]
    when 'pomeranians', 'goldfish'
      v.to_i < mfcsam[k]
    else
      mfcsam[k] == v.to_i
    end
  end
end

puts "Part 1: #{part1[0]}"
puts "Part 2: #{part2[0]}"
