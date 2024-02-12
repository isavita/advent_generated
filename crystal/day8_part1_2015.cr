
def calculate_memory_length(s : String) : Int32
  length = 0
  in_escape = false
  hex_count = 0

  (1..s.size-2).each do |i|
    case
    when hex_count > 0
      hex_count -= 1
    when in_escape
      if s[i] == 'x'
        hex_count = 2
      end
      in_escape = false
      length += 1
    when s[i] == '\\'
      in_escape = true
    else
      length += 1
    end
  end

  length
end

total_diff = 0
File.open("input.txt") do |file|
  file.each_line do |line|
    code_length = line.size
    memory_length = calculate_memory_length(line)
    total_diff += code_length - memory_length
  end
end

puts total_diff
