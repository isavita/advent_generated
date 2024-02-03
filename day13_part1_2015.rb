File.open('input.txt') do |file|
  happiness = Hash.new { |h, k| h[k] = Hash.new(0) }
  people = []

  file.each_line do |line|
    parts = line.scan(/(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)/).flatten
    person1, gain_or_lose, units, person2 = parts

    units = units.to_i
    units *= -1 if gain_or_lose == 'lose'

    happiness[person1][person2] = units
    people << person1
  end

  people.uniq!
  max_happiness = 0

  people.permutation.each do |arrangement|
    happiness_change = (0...arrangement.length).sum do |i|
      happiness[arrangement[i]][arrangement[(i + 1) % arrangement.length]] + happiness[arrangement[(i + 1) % arrangement.length]][arrangement[i]]
    end

    max_happiness = [max_happiness, happiness_change].max
  end

  puts max_happiness
end