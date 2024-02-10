package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	var passports []string
	scanner := bufio.NewScanner(file)
	passport := ""

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			passports = append(passports, strings.TrimSpace(passport))
			passport = ""
		} else {
			passport += " " + line
		}
	}
	if passport != "" {
		passports = append(passports, strings.TrimSpace(passport))
	}

	validPassports := 0

	for _, p := range passports {
		if isValidPassport(p) {
			validPassports++
		}
	}

	fmt.Println(validPassports)
}

func isValidPassport(passport string) bool {
	fields := strings.Fields(passport)
	fieldMap := make(map[string]string)
	for _, field := range fields {
		parts := strings.Split(field, ":")
		fieldMap[parts[0]] = parts[1]
	}

	return validateByr(fieldMap["byr"]) &&
		validateIyr(fieldMap["iyr"]) &&
		validateEyr(fieldMap["eyr"]) &&
		validateHgt(fieldMap["hgt"]) &&
		validateHcl(fieldMap["hcl"]) &&
		validateEcl(fieldMap["ecl"]) &&
		validatePid(fieldMap["pid"])
}

func validateByr(value string) bool {
	return validateYear(value, 1920, 2002)
}

func validateIyr(value string) bool {
	return validateYear(value, 2010, 2020)
}

func validateEyr(value string) bool {
	return validateYear(value, 2020, 2030)
}

func validateYear(value string, min, max int) bool {
	year, err := strconv.Atoi(value)
	if err != nil {
		return false
	}
	return year >= min && year <= max
}

func validateHgt(value string) bool {
	if strings.HasSuffix(value, "cm") {
		hgt, err := strconv.Atoi(strings.TrimSuffix(value, "cm"))
		if err != nil {
			return false
		}
		return hgt >= 150 && hgt <= 193
	} else if strings.HasSuffix(value, "in") {
		hgt, err := strconv.Atoi(strings.TrimSuffix(value, "in"))
		if err != nil {
			return false
		}
		return hgt >= 59 && hgt <= 76
	}
	return false
}

func validateHcl(value string) bool {
	matched, _ := regexp.MatchString(`^#[0-9a-f]{6}$`, value)
	return matched
}

func validateEcl(value string) bool {
	validEcl := map[string]bool{"amb": true, "blu": true, "brn": true, "gry": true, "grn": true, "hzl": true, "oth": true}
	_, valid := validEcl[value]
	return valid
}

func validatePid(value string) bool {
	matched, _ := regexp.MatchString(`^[0-9]{9}$`, value)
	return matched
}