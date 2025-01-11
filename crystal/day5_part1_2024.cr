
def read_input(filename)
  ordering_rules = [] of Array(Int32)
  updates = [] of Array(Int32)
  is_update_section = false

  File.open(filename, "r") do |file|
    file.each_line do |line|
      line = line.strip
      if line.empty?
        is_update_section = true
        next
      end

      if !is_update_section
        parts = line.split("|")
        next unless parts.size == 2
        x = parts[0].strip.to_i?
        y = parts[1].strip.to_i?
        next unless x && y
        ordering_rules << [x.as(Int32), y.as(Int32)]
      else
        nums = line.split(",").map { |num_str| num_str.strip.to_i? }.compact
        updates << nums if nums.size > 0
      end
    end
  end

  {ordering_rules, updates}
end

def is_correctly_ordered(update, rules)
  position = Hash(Int32, Int32).new
  update.each_with_index { |page, idx| position[page] = idx }

  rules.each do |rule|
    x, y = rule
    pos_x = position[x]?
    pos_y = position[y]?
    return false if pos_x && pos_y && pos_x >= pos_y
  end

  true
end

ordering_rules, updates = read_input("input.txt")

sum = 0
updates.each do |update|
  if is_correctly_ordered(update, ordering_rules)
    middle_page = update[update.size // 2]
    sum += middle_page
  end
end

puts sum
