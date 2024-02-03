packages = File.readlines('input.txt').map(&:to_i)
total_weight = packages.sum
group_weight = total_weight / 3

def find_smallest_qe(packages, group_weight, num_packages)
  packages.combination(num_packages).select { |group| group.sum == group_weight }
                                    .map { |group| group.reduce(:*) }
                                    .min
end

num_packages = 1
while true
  qe = find_smallest_qe(packages, group_weight, num_packages)
  break if qe
  num_packages += 1
end

puts qe