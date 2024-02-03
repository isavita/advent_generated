
rules = File.readlines("input.txt")

bag_map = {}
rules.each do |rule|
  outer_bag, inner_bags = rule.split(" bags contain ")
  inner_bags = inner_bags.split(", ")
  
  inner_bags.each do |inner_bag|
    if inner_bag != "no other bags."
      num = inner_bag.split(" ")[0].to_i
      color = inner_bag.split(" ")[1..2].join(" ")
      bag_map[color] ||= []
      bag_map[color] << outer_bag
    end
  end
end

def find_containing_colors(bag_map, color)
  containing_colors = bag_map[color] || []
  containing_colors.each do |containing_color|
    containing_colors += find_containing_colors(bag_map, containing_color)
  end
  containing_colors.uniq
end

puts find_containing_colors(bag_map, "shiny gold").count
