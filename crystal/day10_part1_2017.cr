file = File.open("input.txt", "r")
lengths_str = file.gets
file.close

if lengths_str
  lengths = lengths_str.chomp.split(",").map { |l| l.to_i }

  list = (0..255).to_a
  current_position = 0
  skip_size = 0

  lengths.each do |length|
    (0...length/2).each do |i|
      start_idx = (current_position + i) % 256
      end_idx = (current_position + length - 1 - i) % 256
      list[start_idx], list[end_idx] = list[end_idx], list[start_idx]
    end

    current_position = (current_position + length + skip_size) % 256
    skip_size += 1
  end

  result = list[0] * list[1]
  puts result
else
  puts "Error: No data found in the input file."
end