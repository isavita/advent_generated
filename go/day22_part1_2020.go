package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var player1Deck, player2Deck []int
	currentDeck := &player1Deck
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			currentDeck = &player2Deck
			continue
		}
		if strings.Contains(line, "Player") {
			continue
		}
		card, _ := strconv.Atoi(line)
		*currentDeck = append(*currentDeck, card)
	}

	for len(player1Deck) > 0 && len(player2Deck) > 0 {
		card1, card2 := player1Deck[0], player2Deck[0]
		player1Deck, player2Deck = player1Deck[1:], player2Deck[1:]
		if card1 > card2 {
			player1Deck = append(player1Deck, card1, card2)
		} else {
			player2Deck = append(player2Deck, card2, card1)
		}
	}

	var winningDeck []int
	if len(player1Deck) > 0 {
		winningDeck = player1Deck
	} else {
		winningDeck = player2Deck
	}

	score := 0
	for i, card := range winningDeck {
		score += card * (len(winningDeck) - i)
	}
	fmt.Println(score)
}