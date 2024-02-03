
packages = File.readlines('input.txt').map(&:to_i)
group_weight = packages.sum / 4

def find_groups(packages, group_weight, num_groups)
  packages.combination(num_groups).select { |group| group.sum == group_weight }
end

def quantum_entanglement(group)
  group.reduce(:*)
end

def find_min_qe(groups)
  groups.map { |group| quantum_entanglement(group) }.min
end

(1..packages.length).each do |num_packages|
  groups = find_groups(packages, group_weight, num_packages)
  next if groups.empty?

  min_qe = find_min_qe(groups)
  puts min_qe
  break
end
