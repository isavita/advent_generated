
def decompressed_length_v1(input)
  marker = /\((\d+)x(\d+)\)/
  length = 0
  pos = 0

  while pos < input.length
    if input[pos] == "("
      marker_match = input[pos..].match(marker)
      if marker_match
        marker_length = marker_match[1].to_i
        repeat_times = marker_match[2].to_i
        pos += marker_match[0].length + marker_length
        length += marker_length * repeat_times
      else
        length += 1
        pos += 1
      end
    else
      length += 1
      pos += 1
    end
  end

  length
end

def decompressed_length_v2(input)
  marker = /\((\d+)x(\d+)\)/
  length = 0
  pos = 0

  while pos < input.length
    if input[pos] == "("
      marker_match = input[pos..].match(marker)
      if marker_match
        marker_length = marker_match[1].to_i
        repeat_times = marker_match[2].to_i
        pos += marker_match[0].length + marker_length
        length += decompressed_length_v2(input[pos-marker_length...pos]) * repeat_times
      else
        length += 1
        pos += 1
      end
    else
      length += 1
      pos += 1
    end
  end

  length
end

input = File.read("input.txt").gsub(/\s+/, "")
puts decompressed_length_v1(input)
puts decompressed_length_v2(input)
