passports = File.read('input.txt').split("\n\n")

valid_passports = 0

passports.each do |passport|
  fields = passport.split(/[ \n]/).map { |field| field.split(':').first }
  valid_passports += 1 if %w(byr iyr eyr hgt hcl ecl pid).all? { |field| fields.include?(field) }
end

puts valid_passports