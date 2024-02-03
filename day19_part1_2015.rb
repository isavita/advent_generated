
replacements = []
molecule = nil

File.foreach("input.txt") do |line|
  if line =~ /(\w+) => (\w+)/
    replacements << [$1, $2]
  elsif line =~ /^\w+$/
    molecule = line.strip
  end
end

results = []

replacements.each do |from, to|
  start = 0
  while idx = molecule.index(from, start)
    results << molecule[0...idx] + to + molecule[idx + from.length..-1]
    start = idx + 1
  end
end

puts results.uniq.size
