File.open("input.txt", "r") do |file|
  passports = file.read.split("\n\n")
  
  valid_passports = 0
  
  passports.each do |passport|
    fields = passport.split(/[ \n]/).map { |field| field.split(":") }.to_h
    
    next unless fields.keys.size == 8 || (fields.keys.size == 7 && !fields.key?("cid"))
    
    valid_passport = true
    
    valid_passport &&= fields["byr"].to_i >= 1920 && fields["byr"].to_i <= 2002
    valid_passport &&= fields["iyr"].to_i >= 2010 && fields["iyr"].to_i <= 2020
    valid_passport &&= fields["eyr"].to_i >= 2020 && fields["eyr"].to_i <= 2030
    
    if fields["hgt"].include?("cm")
      valid_passport &&= fields["hgt"].to_i >= 150 && fields["hgt"].to_i <= 193
    elsif fields["hgt"].include?("in")
      valid_passport &&= fields["hgt"].to_i >= 59 && fields["hgt"].to_i <= 76
    else
      valid_passport = false
    end
    
    valid_passport &&= fields["hcl"].match?(/^#[0-9a-f]{6}$/)
    valid_passport &&= ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].include?(fields["ecl"])
    valid_passport &&= fields["pid"].match?(/^[0-9]{9}$/)
    
    valid_passports += 1 if valid_passport
  end
  
  puts valid_passports
end