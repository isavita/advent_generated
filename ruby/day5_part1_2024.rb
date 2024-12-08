
def read_input(filename)
  ordering_rules = []
  updates = []
  reading_updates = false

  File.open(filename).each_line do |line|
    line.strip!
    if line.empty?
      reading_updates = true
      next
    end

    if reading_updates
      updates << line.split(',').map(&:to_i)
    else
      x, y = line.split('|').map(&:strip).map(&:to_i)
      ordering_rules << [x, y]
    end
  end
  [ordering_rules, updates]
end

def is_correctly_ordered(update, rules)
  position = update.each_with_index.to_h
  rules.all? do |x, y|
    !(position.key?(x) && position.key?(y) && position[x] >= position[y])
  end
end

ordering_rules, updates = read_input("input.txt")

sum = updates.sum do |update|
  is_correctly_ordered(update, ordering_rules) ? update[update.size / 2] : 0
end

puts sum
