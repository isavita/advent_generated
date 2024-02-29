
#!/bin/bash

file="input.txt"
required_fields=("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")
valid_passports=0
passport=""

while IFS= read -r line || [[ -n "$line" ]]; do
  if [ -z "$line" ]; then
    passports+=("$passport")
    passport=""
  else
    passport+=" $line"
  fi
done < "$file"

if [ -n "$passport" ]; then
  passports+=("$passport")
fi

for passport in "${passports[@]}"; do
  is_valid=true
  for field in "${required_fields[@]}"; do
    if ! echo "$passport" | grep -q "$field:"; then
      is_valid=false
      break
    fi
  done
  if $is_valid; then
    ((valid_passports++))
  fi
done

echo "$valid_passports"
