
total_diff = File.readlines('input.txt').sum do |line|
  line.chomp!
  code_length = line.length
  memory_length = line[1..-2].gsub(/\\x[0-9a-f]{2}|\\./, '_').length
  code_length - memory_length
end

puts total_diff
