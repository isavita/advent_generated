def count_combinations(containers, target, index = 0, current_sum = 0, memo = {})
  key = [index, current_sum]
  return memo[key] if memo.key?(key)

  if current_sum == target
    return 1
  elsif current_sum > target || index == containers.length
    return 0
  end

  # Count combinations including the current container
  include_current = count_combinations(containers, target, index + 1, current_sum + containers[index], memo)
  
  # Count combinations excluding the current container
  exclude_current = count_combinations(containers, target, index + 1, current_sum, memo)

  memo[key] = include_current + exclude_current
  memo[key]
end

# Read container sizes from input file
containers = File.readlines('input.txt').map(&:to_i)

# Calculate and print the result
result = count_combinations(containers, 150)
puts result
