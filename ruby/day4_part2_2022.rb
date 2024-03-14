File.open("input.txt", "r") do |file|
  count = 0
  file.each_line do |line|
    pair = line.strip.split(",")
    left = pair[0].split("-").map(&:to_i)
    right = pair[1].split("-").map(&:to_i)
    count += 1 if left[0] <= right[1] && left[1] >= right[0]
  end
  puts count
end