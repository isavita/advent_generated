
input = File.read("input.txt")
re = /mul\((\d{1,3}),(\d{1,3})\)|(do\(\))|(don't\(\))/
enabled = true
total = 0_i64
input.scan(re) do |m|
  if m[1]?
    total += m[1].to_i * m[2].to_i if enabled
  elsif m[3]?
    enabled = true
  elsif m[4]?
    enabled = false
  end
end
puts total
