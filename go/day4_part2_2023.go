package main

import (
	"fmt"
	"os"
	"regexp"
	"strings"
)

type Card struct {
	winnings   map[string]int
	givens     map[string]int
	totalCount int
}

func getPointsForCard(card Card) int {
	points := 0
	for given, count := range card.givens {
		if winningCount, ok := card.winnings[given]; ok {
			points += count * winningCount
		}
	}
	return points
}

func lexLineIntoCard(line string) Card {
	_, cardDataStr, _ := strings.Cut(line, ": ")
	cardData := strings.Split(cardDataStr, " | ")

	re := regexp.MustCompile("[0-9]{1,2}")

	winnings := make(map[string]int)
	for _, point := range re.FindAllString(cardData[0], -1) {
		winnings[point]++
	}

	givens := make(map[string]int)
	for _, point := range re.FindAllString(cardData[1], -1) {
		givens[point]++
	}

	return Card{
		winnings:   winnings,
		givens:     givens,
		totalCount: 1,
	}
}

func main() {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}
	input := strings.TrimSpace(string(file))

	var cards []Card

	for _, line := range strings.Split(input, "\n") {
		if len(line) == 0 {
			continue
		}
		card := lexLineIntoCard(line)
		cards = append(cards, card)
	}

	for i, card := range cards {
		points := getPointsForCard(card)

		for j := 1; j <= points; j++ {
			cards[i+j].totalCount += 1 * cards[i].totalCount
		}
	}

	totalCards := 0
	for _, card := range cards {
		totalCards += card.totalCount
	}

	fmt.Println(totalCards)
}