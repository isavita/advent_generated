def is_real_room(room)
  parts = room.split('[')
  checksum = parts[1].chomp(']')
  encrypted_name = parts[0].split('-')[0..-2]

  letter_counts = Hash.new(0)
  encrypted_name.each do |part|
    part.each_char { |letter| letter_counts[letter] += 1 }
  end

  sorted_counts = letter_counts.sort_by { |letter, count| [-count, letter] }

  checksum.chars.each_with_index do |char, index|
    return false if char != sorted_counts[index][0]
  end

  true
end

def get_sector_id(room)
  parts = room.split('-')
  sector_id_part = parts.last
  sector_id = sector_id_part.split('[')[0].to_i
  sector_id
end

sum_of_sector_ids = 0
File.open('input.txt', 'r') do |file|
  file.each_line do |line|
    line.chomp!
    if is_real_room(line)
      sum_of_sector_ids += get_sector_id(line)
    end
  end
end

puts sum_of_sector_ids