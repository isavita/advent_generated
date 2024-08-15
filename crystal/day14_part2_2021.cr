require "file"
require "json"

def main
  template, rules = read_input("input.txt")
  pair_counts = Hash(String, Int64).new(0)

  (0...template.size - 1).each do |i|
    pair_counts[template[i, 2]] += 1
  end

  40.times do
    new_pair_counts = Hash(String, Int64).new(0)
    pair_counts.each do |pair, count|
      if rules.has_key?(pair)
        insert = rules[pair]
        new_pair_counts[pair[0] + insert] += count
        new_pair_counts[insert + pair[1]] += count
      else
        new_pair_counts[pair] += count
      end
    end
    pair_counts = new_pair_counts
  end

  element_counts = Hash(Char, Int64).new(0)
  pair_counts.each do |pair, count|
    element_counts[pair[0]] += count
  end
  element_counts[template[-1]] += 1

  max_count = element_counts.values.max
  min_count = element_counts.values.min

  puts max_count - min_count
end

def read_input(filename)
  lines = File.read(filename).lines.map { |line| line.chomp }
  template = lines.shift
  rules = Hash(String, String).new

  lines.each do |line|
    next if line.empty?
    parts = line.split(" -> ")
    rules[parts[0]] = parts[1]
  end

  return template, rules
end

main