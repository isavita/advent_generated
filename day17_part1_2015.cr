
def count_combinations(containers : Array(Int32), target : Int32, index : Int32) : Int32
  return 1 if target == 0
  return 0 if target < 0 || index >= containers.size

  count_combinations(containers, target - containers[index], index + 1) +
    count_combinations(containers, target, index + 1)
end

containers = File.read("input.txt").lines.map { |line| line.to_i }

puts count_combinations(containers, 150, 0)
