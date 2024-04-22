containers = File.read_lines("input.txt").map(&.to_i)
target = 150

combinations = Hash(Int32, Int32).new

def combinations_sum(containers, target, current_combination, current_sum, current_index, combinations)
  if current_sum == target
    combinations[current_combination.size] ||= 0
    combinations[current_combination.size] += 1
  elsif current_sum > target
    return
  else
    (current_index...containers.size).each do |i|
      combinations_sum(containers, target, current_combination + [containers[i]], current_sum + containers[i], i, combinations)
    end
  end
end

combinations_sum(containers, target, [] of Int32, 0, 0, combinations)

puts "Part One: #{combinations.values.sum}"
puts "Part Two: #{combinations[combinations.keys.min]}"