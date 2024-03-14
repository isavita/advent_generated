def read_input(filename)
  template, rules = File.read(filename).split("\n\n")
  template = template.strip
  rules = rules.split("\n").map { |line| line.split(" -> ") }.to_h
  [template, rules]
end

def solve(template, rules)
  pair_counts = Hash.new(0)
  template.each_char.each_cons(2) { |pair| pair_counts[pair.join] += 1 }

  40.times do
    new_pair_counts = Hash.new(0)
    pair_counts.each do |pair, count|
      if rules.key?(pair)
        insert = rules[pair]
        new_pair_counts[pair[0] + insert] += count
        new_pair_counts[insert + pair[1]] += count
      else
        new_pair_counts[pair] += count
      end
    end
    pair_counts = new_pair_counts
  end

  element_counts = Hash.new(0)
  pair_counts.each do |pair, count|
    element_counts[pair[0]] += count
  end
  element_counts[template[-1]] += 1

  element_counts.values.max - element_counts.values.min
end

template, rules = read_input("input.txt")
puts solve(template, rules)