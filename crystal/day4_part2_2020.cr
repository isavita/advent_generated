file = File.open("input.txt")
passports = [] of String
passport = ""

file.each_line do |line|
  if line.strip.empty?
    passports << passport.strip
    passport = ""
  else
    passport += " " + line
  end
end
if !passport.empty?
  passports << passport.strip
end

valid_passports = 0

passports.each do |p|
  if is_valid_passport(p)
    valid_passports += 1
  end
end

puts valid_passports

def is_valid_passport(passport)
  fields = passport.split
  field_map = {} of String => String
  fields.each do |field|
    parts = field.split(":")
    field_map[parts[0]] = parts[1]
  end

  byr = field_map["byr"]?
  iyr = field_map["iyr"]?
  eyr = field_map["eyr"]?
  hgt = field_map["hgt"]?
  hcl = field_map["hcl"]?
  ecl = field_map["ecl"]?
  pid = field_map["pid"]?

  return false if byr.nil? || iyr.nil? || eyr.nil? || hgt.nil? || hcl.nil? || ecl.nil? || pid.nil?

  validate_byr(byr) &&
    validate_iyr(iyr) &&
    validate_eyr(eyr) &&
    validate_hgt(hgt) &&
    validate_hcl(hcl) &&
    validate_ecl(ecl) &&
    validate_pid(pid)
end

def validate_byr(value)
  validate_year(value, 1920, 2002)
end

def validate_iyr(value)
  validate_year(value, 2010, 2020)
end

def validate_eyr(value)
  validate_year(value, 2020, 2030)
end

def validate_year(value, min, max)
  year = value.to_i
  year >= min && year <= max
rescue
  false
end

def validate_hgt(value)
  if value.ends_with?("cm")
    hgt = value[0...-2].to_i
    hgt >= 150 && hgt <= 193
  elsif value.ends_with?("in")
    hgt = value[0...-2].to_i
    hgt >= 59 && hgt <= 76
  else
    false
  end
end

def validate_hcl(value)
  value =~ /^#[0-9a-f]{6}$/
end

def validate_ecl(value)
  %w(amb blu brn gry grn hzl oth).includes?(value)
end

def validate_pid(value)
  value =~ /^[0-9]{9}$/
end