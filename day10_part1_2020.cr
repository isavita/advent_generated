
adapters = File.read("input.txt").lines.map { |line| line.to_i }.sort
jolt_differences = {1 => 0, 3 => 0}
previous_joltage = 0

adapters.each do |adapter|
  diff = adapter - previous_joltage
  jolt_differences[diff] += 1
  previous_joltage = adapter
end

product = jolt_differences[1] * (jolt_differences[3] + 1) # Adding 1 to the 3 difference count for the device's built-in adapter
puts product
