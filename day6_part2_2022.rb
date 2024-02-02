
data = File.read('input.txt').chomp

def find_marker_position(data, marker_length)
  data.chars.each_with_index do |char, index|
    next if index < marker_length - 1

    if data[index - marker_length + 1..index].chars.uniq.length == marker_length
      return index + 1
    end
  end
end

puts find_marker_position(data, 14)
