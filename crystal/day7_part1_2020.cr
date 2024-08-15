require "json"
require "set"

def main
  contains = Hash(String, Array(String)).new
  File.open("input.txt") do |file|
    file.each_line do |line|
      parts = line.split(" bags contain ")
      container = parts[0]
      next if parts[1] == "no other bags."

      contained_bags = parts[1].split(", ")
      contained_bags.each do |bag|
        bag_name = bag.split[1..2].join(" ")
        contains[bag_name] ||= [] of String
        contains[bag_name] << container
      end
    end
  end

  count = count_can_contain("shiny gold", contains)
  puts count
end

def count_can_contain(target, contains)
  seen = Set(String).new
  dfs(target, contains, seen)
  seen.size
end

def dfs(bag, contains, seen)
  return unless contains.has_key?(bag)

  contains[bag].each do |outer|
    unless seen.includes?(outer)
      seen.add(outer)
      dfs(outer, contains, seen)
    end
  end
end

main