
data = File.readlines('input.txt').map(&:chomp)

def real_room?(name, checksum)
  name.gsub('-', '').chars.group_by(&:itself).transform_values(&:size).sort_by { |k, v| [-v, k] }.map(&:first)[0, 5].join == checksum
end

def decrypt_name(name, shift)
  name.tr('a-z', ('a'..'z').to_a.rotate(shift).join)
end

sum = 0

data.each do |line|
  parts = line.match(/([a-z-]+)-(\d+)\[([a-z]+)\]/)
  name = parts[1]
  sector_id = parts[2].to_i
  checksum = parts[3]

  if real_room?(name, checksum)
    decrypted_name = decrypt_name(name, sector_id)
    if decrypted_name.include?('northpole')
      puts sector_id
      break
    end
  end
end
