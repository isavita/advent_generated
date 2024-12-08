
input = File.read("input.txt")
re = /(mul\(\d{1,3},\d{1,3}\))|(do\(\))|(don't\(\))/
matches = input.scan(re)

enabled = true
total_sum = 0

matches.each do |match|
  if match[0]
    if enabled
      x, y = match[0][4..-2].split(',').map(&:to_i)
      total_sum += x * y
    end
  elsif match[1]
    enabled = true
  elsif match[2]
    enabled = false
  end
end

puts total_sum
