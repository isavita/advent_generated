
def find_smallest_qe(weights : Array(Int32), num_groups : Int32)
  total_weight = weights.sum
  raise "Weights cannot be split into #{num_groups} equal groups" unless total_weight % num_groups == 0
  target_weight = total_weight // num_groups

  best_qe = Int64::MAX
  min_count = Int32::MAX

  (1..weights.size).each do |count|
    weights.combinations(count).each do |comb|
      next unless comb.sum == target_weight

      if num_groups == 3
        remaining_weights = weights - comb
        valid_split = false
        
        (1..(remaining_weights.size // 2)).each do |count2|
          remaining_weights.combinations(count2).each do |comb2|
            if comb2.sum == target_weight
              valid_split = true
              break
            end
          end
          break if valid_split
        end
        next unless valid_split
      end
      

      qe = comb.map(&.to_i64).product
      
      if count < min_count
        min_count = count
        best_qe = qe
      elsif count == min_count && qe < best_qe
        best_qe = qe
      end      
    end
    break if min_count != Int32::MAX # Optimization: Stop once the first valid combinations have been found, as combinations are evaluated in order by size.
  end
  best_qe
end

def main
  begin
    weights = [] of Int32
    File.open("./input.txt") do |file|
      file.each_line do |line|
        weights << line.strip.to_i
      end
    end
    
    puts find_smallest_qe(weights, 3)

  rescue e : File::Error
    puts "Error reading file: #{e.message}"
  rescue e : Exception
    puts "Error: #{e.message}"
  end
end

main
