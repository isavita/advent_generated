package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type Rule struct {
	name   string
	ranges [][2]int
}

func (r *Rule) isValid(value int) bool {
	for _, rng := range r.ranges {
		if value >= rng[0] && value <= rng[1] {
			return true
		}
	}
	return false
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("File reading error:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	rules := make([]Rule, 0)
	myTicket := make([]int, 0)
	nearbyTickets := make([][]int, 0)
	section := 0
	reRule := regexp.MustCompile(`^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$`)

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			section++
			continue
		}
		switch section {
		case 0:
			parts := reRule.FindStringSubmatch(line)
			if parts != nil {
				rule := Rule{
					name: parts[1],
					ranges: [][2]int{
						{toInt(parts[2]), toInt(parts[3])},
						{toInt(parts[4]), toInt(parts[5])},
					},
				}
				rules = append(rules, rule)
			}
		case 1:
			if line != "your ticket:" {
				myTicket = parseTicket(line)
			}
		case 2:
			if line != "nearby tickets:" {
				ticket := parseTicket(line)
				if isValidTicket(ticket, rules) {
					nearbyTickets = append(nearbyTickets, ticket)
				}
			}
		}
	}

	fieldPositions := solveFieldPositions(rules, nearbyTickets)
	departureProduct := calculateDepartureProduct(myTicket, fieldPositions)

	fmt.Println(departureProduct)
}

func toInt(s string) int {
	value, _ := strconv.Atoi(s)
	return value
}

func parseTicket(s string) []int {
	values := strings.Split(s, ",")
	ticket := make([]int, len(values))
	for i, v := range values {
		ticket[i] = toInt(v)
	}
	return ticket
}

func isValidTicket(ticket []int, rules []Rule) bool {
	for _, value := range ticket {
		if !isValidForAnyRule(value, rules) {
			return false
		}
	}
	return true
}

func isValidForAnyRule(value int, rules []Rule) bool {
	for _, rule := range rules {
		if rule.isValid(value) {
			return true
		}
	}
	return false
}

func solveFieldPositions(rules []Rule, tickets [][]int) map[string]int {
	validPositions := make(map[string]map[int]bool)
	for _, rule := range rules {
		validPositions[rule.name] = make(map[int]bool)
		for i := 0; i < len(tickets[0]); i++ {
			valid := true
			for _, ticket := range tickets {
				if !rule.isValid(ticket[i]) {
					valid = false
					break
				}
			}
			if valid {
				validPositions[rule.name][i] = true
			}
		}
	}

	fieldPositions := make(map[string]int)
	for len(fieldPositions) < len(rules) {
		for name, positions := range validPositions {
			if len(positions) == 1 {
				for pos := range positions {
					fieldPositions[name] = pos
					for otherName := range validPositions {
						delete(validPositions[otherName], pos)
					}
				}
				delete(validPositions, name)
			}
		}
	}
	return fieldPositions
}

func calculateDepartureProduct(ticket []int, fieldPositions map[string]int) int {
	product := 1
	for name, pos := range fieldPositions {
		if strings.HasPrefix(name, "departure") {
			product *= ticket[pos]
		}
	}
	return product
}