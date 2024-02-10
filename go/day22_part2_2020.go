package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Deck []int

func (d Deck) Copy(n int) Deck {
	copy := make(Deck, n)
	for i := 0; i < n; i++ {
		copy[i] = d[i]
	}
	return copy
}

func (d Deck) Score() int {
	score := 0
	for i, card := range d {
		score += card * (len(d) - i)
	}
	return score
}

func playRecursiveCombat(player1, player2 Deck) (Deck, Deck) {
	previousRounds := make(map[string]bool)
	for len(player1) > 0 && len(player2) > 0 {
		roundKey := fmt.Sprintf("%v|%v", player1, player2)
		if previousRounds[roundKey] {
			return player1, Deck{}
		}
		previousRounds[roundKey] = true

		card1, card2 := player1[0], player2[0]
		player1, player2 = player1[1:], player2[1:]

		if len(player1) >= card1 && len(player2) >= card2 {
			subPlayer1, _ := playRecursiveCombat(player1.Copy(card1), player2.Copy(card2))
			if len(subPlayer1) > 0 {
				player1 = append(player1, card1, card2)
			} else {
				player2 = append(player2, card2, card1)
			}
		} else {
			if card1 > card2 {
				player1 = append(player1, card1, card2)
			} else {
				player2 = append(player2, card2, card1)
			}
		}
	}
	return player1, player2
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var player1Deck, player2Deck Deck
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

	player1Deck, player2Deck = playRecursiveCombat(player1Deck, player2Deck)

	var winningDeck Deck
	if len(player1Deck) > 0 {
		winningDeck = player1Deck
	} else {
		winningDeck = player2Deck
	}

	fmt.Println(winningDeck.Score())
}