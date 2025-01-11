
def decompressed_length(input : String, version : Int32) : Int64
  length = 0_i64
  i = 0
  while i < input.size
    if input[i] == '('
      j = input.index(')', i + 1).not_nil!
      marker = input[i + 1..j - 1]
      chars, repeat = marker.split('x').map(&.to_i)
      i = j + 1
      if version == 1
        length += chars * repeat
        i += chars
      else
        length += decompressed_length(input[i..i + chars - 1], version) * repeat
        i += chars
      end
    else
      length += 1
      i += 1
    end
  end
  length
end

input = File.read("input.txt").strip

puts decompressed_length(input, 1)
puts decompressed_length(input, 2)
