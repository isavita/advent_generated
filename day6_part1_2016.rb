
input = File.readlines("input.txt").map(&:chomp)

message_length = input.first.length
message = ""

(0..message_length-1).each do |i|
  char_count = Hash.new(0)
  input.each do |line|
    char_count[line[i]] += 1
  end
  message += char_count.max_by { |k, v| v }[0]
end

puts message
