def decompress(data)
  result = ""
  i = 0
  while i < data.size
    if data[i] == '('
      end_marker = data.index(')', i)
      if end_marker.nil?
        raise "Malformed input: missing closing parenthesis"
      end
      marker = data[i+1..end_marker-1]
      length, repeat = marker.split('x').map(&.to_i)
      result += data[end_marker+1, length] * repeat
      i = end_marker + 1 + length
    else
      result += data[i]
      i += 1
    end
  end
  result
end

File.open("input.txt", "r") do |file|
  data = file.gets.not_nil!.chomp
  puts decompress(data).size
end