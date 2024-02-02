
relationships = Hash.new { |h, k| h[k] = Hash.new(0) }
people = []

File.open("input.txt").each do |line|
  line.chomp.match(/(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)/) do |match|
    relationships[match[1]][match[4]] = match[2] == "gain" ? match[3].to_i : -match[3].to_i
    people << match[1]
  end
end

people.uniq!

def calculate_happiness(people, relationships)
  people.permutation.map do |arrangement|
    happiness = 0
    arrangement.each_with_index do |person, index|
      left = index == 0 ? arrangement[-1] : arrangement[index - 1]
      right = index == arrangement.length - 1 ? arrangement[0] : arrangement[index + 1]
      happiness += relationships[person][left] + relationships[person][right]
    end
    happiness
  end.max
end

puts calculate_happiness(people, relationships)

people.each { |person| relationships[person]['You'] = 0 }
relationships['You'] = Hash.new(0)
people << 'You'

puts calculate_happiness(people, relationships)
