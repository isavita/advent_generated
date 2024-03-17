file = File.open("input.txt")
passports = [] of String
passport = ""

file.each_line do |line|
  if line.strip.empty?
    passports << passport
    passport = ""
  else
    passport += " " + line
  end
end

passports << passport unless passport.empty?

valid_passports = 0
required_fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

passports.each do |p|
  valid_passports += 1 if is_valid(p, required_fields)
end

puts valid_passports

def is_valid(passport, required_fields)
  required_fields.all? { |field| passport.includes?("#{field}:") }
end