def solve(steps)
  input = File.read('input.txt').split("\n")
  template = input.shift
  input.shift  # Remove empty line

  rules = input.each_with_object({}) do |line, hash|
    pair, insert = line.split(' -> ')
    hash[pair] = insert
  end

  pair_count = Hash.new(0)
  char_count = Hash.new(0)

  template.each_char { |c| char_count[c] += 1 }
  template.chars.each_cons(2) { |a, b| pair_count[a + b] += 1 }

  steps.times do
    new_pair_count = Hash.new(0)
    pair_count.each do |pair, count|
      if rules.key?(pair)
        insert = rules[pair]
        new_pair_count[pair[0] + insert] += count
        new_pair_count[insert + pair[1]] += count
        char_count[insert] += count
      else
        new_pair_count[pair] += count
      end
    end
    pair_count = new_pair_count
  end

  most_common = char_count.values.max
  least_common = char_count.values.min
  most_common - least_common
end

puts solve(10)  # Part 1
puts solve(40)  # Part 2
