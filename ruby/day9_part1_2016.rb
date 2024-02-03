input = File.read('input.txt').chomp

def decompressed_length(input)
  marker = /\((\d+)x(\d+)\)/
  length = 0

  while input.match(marker)
    length += $`.length
    input = $'
    chars, repeat = $1.to_i, $2.to_i
    length += chars * repeat
    input = input[chars..-1]
  end

  length + input.length
end

puts decompressed_length(input)