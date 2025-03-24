
def find_best_group(weights : Array(Int32), groups_count : Int32)
  total_weight = weights.sum
  target_weight = total_weight // groups_count

  best_qe = Int64::MAX
  min_count = Int32::MAX

  (1..weights.size).each do |count|
    weights.combinations(count).each do |group|
      next if group.sum != target_weight

      qe = group.reduce(1_i64) { |acc, w| acc * w.to_i64 }

      if count < min_count
        min_count = count
        best_qe = qe
      elsif count == min_count && qe < best_qe
        best_qe = qe
      end
    end
    break if min_count < Int32::MAX # Optimization: Stop if a solution for current count is found
  end

  best_qe
end

def main
  file = File.open("input.txt")
  weights = [] of Int32
  file.each_line do |line|
    weights << line.strip.to_i32
  end
  file.close

  puts "Part 1: #{find_best_group(weights, 3)}"
  puts "Part 2: #{find_best_group(weights, 4)}"
end

main
