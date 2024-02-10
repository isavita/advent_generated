package main

import (
	"bufio"
	"fmt"
	"os"
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
			passports = append(passports, passport)
			passport = ""
		} else {
			passport += " " + line
		}
	}
	if passport != "" {
		passports = append(passports, passport)
	}

	validPassports := 0
	requiredFields := []string{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}

	for _, p := range passports {
		if isValid(p, requiredFields) {
			validPassports++
		}
	}

	fmt.Println(validPassports)
}

func isValid(passport string, requiredFields []string) bool {
	for _, field := range requiredFields {
		if !strings.Contains(passport, field+":") {
			return false
		}
	}
	return true
}