containers = File.readlines('input.txt').map(&:to_i)
target = 150

def find_combinations(containers, target)
  (1..containers.size).flat_map { |n| containers.combination(n).select { |c| c.sum == target } }
end

combinations = find_combinations(containers, target)
puts combinations.size

min_containers = combinations.min_by(&:size).size
min_combinations = combinations.count { |c| c.size == min_containers }
puts min_combinations