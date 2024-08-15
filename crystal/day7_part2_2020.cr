require "file_utils"

class BagRule
  property color : String
  property count : Int32

  def initialize(@color, @count)
  end
end

def count_bags(color, rules)
  return 0 unless rules[color]?

  count = 1
  rules[color].each do |rule|
    count += rule.count * count_bags(rule.color, rules)
  end
  count
end

file = File.open("input.txt")
rules = Hash(String, Array(BagRule)).new

file.each_line do |line|
  parts = line.split(" bags contain ")
  container = parts[0]
  contents = parts[1]

  next if contents == "no other bags.\n"

  contents.scan(/(\d+) (\w+ \w+) bags?[,.]/) do |match|
    count = match[1].to_i
    rules[container] ||= Array(BagRule).new
    rules[container] << BagRule.new(match[2], count)
  end
end

# Add all colors to the rules hash
rules.keys.each do |container|
  rules[container].each do |rule|
    rules[rule.color] ||= Array(BagRule).new
  end
end

total_bags = count_bags("shiny gold", rules) - 1
puts total_bags