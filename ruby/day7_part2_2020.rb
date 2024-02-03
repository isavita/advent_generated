
rules = Hash.new { |h, k| h[k] = [] }
rule_regex = /(\d+) (\w+ \w+) bags?[,.]/

File.open("input.txt").each do |line|
  container, contents = line.chomp.split(" bags contain ")
  next if contents == "no other bags."

  contents.scan(rule_regex) do |count, color|
    rules[container] << { color: color, count: count.to_i }
  end
end

def count_bags(color, rules)
  count = 1
  rules[color].each do |rule|
    count += rule[:count] * count_bags(rule[:color], rules)
  end
  count
end

total_bags = count_bags("shiny gold", rules) - 1
puts total_bags
