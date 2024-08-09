validate_year <- function(value, min, max) {
  year <- as.integer(value)
  !is.na(year) && year >= min && year <= max
}

validate_hgt <- function(value) {
  if (grepl("cm$", value)) {
    hgt <- as.integer(sub("cm$", "", value))
    return(!is.na(hgt) && hgt >= 150 && hgt <= 193)
  } else if (grepl("in$", value)) {
    hgt <- as.integer(sub("in$", "", value))
    return(!is.na(hgt) && hgt >= 59 && hgt <= 76)
  }
  FALSE
}

validate_hcl <- function(value) {
  grepl("^#[0-9a-f]{6}$", value)
}

validate_ecl <- function(value) {
  value %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
}

validate_pid <- function(value) {
  grepl("^[0-9]{9}$", value)
}

is_valid_passport <- function(passport) {
  fields <- unlist(strsplit(passport, "\\s+"))
  field_map <- setNames(sub(".*:", "", fields), sub(":.*", "", fields))
  
  validate_year(field_map["byr"], 1920, 2002) &&
    validate_year(field_map["iyr"], 2010, 2020) &&
    validate_year(field_map["eyr"], 2020, 2030) &&
    validate_hgt(field_map["hgt"]) &&
    validate_hcl(field_map["hcl"]) &&
    validate_ecl(field_map["ecl"]) &&
    validate_pid(field_map["pid"])
}

main <- function() {
  passports <- readLines("input.txt")
  passports <- unlist(strsplit(paste(passports, collapse = "\n"), "\n\n"))
  valid_passports <- sum(sapply(trimws(passports), is_valid_passport))
  print(valid_passports)
}

main()