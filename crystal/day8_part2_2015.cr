File.open("input.txt", "r") do |file|
  total_code_length = 0
  total_encoded_length = 0
  file.each_line do |line|
    code_length = line.size
    encoded_length = "\"#{line.chomp.gsub("\\", "\\\\").gsub("\"", "\\\"")}\""
    total_code_length += code_length
    total_encoded_length += encoded_length.size
  end
  puts total_encoded_length - total_code_length
end