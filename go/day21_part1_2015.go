package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Item struct {
	Cost   int
	Damage int
	Armor  int
}

type Character struct {
	HitPoints int
	Damage    int
	Armor     int
}

func main() {
	// Read the boss's stats from the file.
	data, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	lines := strings.Split(string(data), "\n")
	boss := Character{
		HitPoints: parseStat(lines[0]),
		Damage:    parseStat(lines[1]),
		Armor:     parseStat(lines[2]),
	}

	weapons := []Item{
		{Cost: 8, Damage: 4},
		{Cost: 10, Damage: 5},
		{Cost: 25, Damage: 6},
		{Cost: 40, Damage: 7},
		{Cost: 74, Damage: 8},
	}

	armors := []Item{
		{Cost: 0, Armor: 0}, // No armor option
		{Cost: 13, Armor: 1},
		{Cost: 31, Armor: 2},
		{Cost: 53, Armor: 3},
		{Cost: 75, Armor: 4},
		{Cost: 102, Armor: 5},
	}

	rings := []Item{
		{Cost: 0}, // No ring option
		{Cost: 25, Damage: 1},
		{Cost: 50, Damage: 2},
		{Cost: 100, Damage: 3},
		{Cost: 20, Armor: 1},
		{Cost: 40, Armor: 2},
		{Cost: 80, Armor: 3},
	}

	minCost := int(^uint(0) >> 1) // Set to maximum int value.
	for _, w := range weapons {
		for _, a := range armors {
			for ri := 0; ri < len(rings); ri++ {
				for rj := ri + 1; rj < len(rings); rj++ {
					player := Character{HitPoints: 100, Damage: w.Damage, Armor: a.Armor}
					player.Damage += rings[ri].Damage + rings[rj].Damage
					player.Armor += rings[ri].Armor + rings[rj].Armor
					cost := w.Cost + a.Cost + rings[ri].Cost + rings[rj].Cost
					if playerWins(player, boss) && cost < minCost {
						minCost = cost
					}
				}
			}
		}
	}

	fmt.Printf("%d\n", minCost)
}

func parseStat(line string) int {
	parts := strings.Split(line, ": ")
	value, err := strconv.Atoi(parts[1])
	if err != nil {
		log.Fatalf("Invalid stat in input: %s", parts[1])
	}
	return value
}

func playerWins(player, boss Character) bool {
	playerDamage := max(1, player.Damage-boss.Armor)
	bossDamage := max(1, boss.Damage-player.Armor)

	playerTurns := (boss.HitPoints + playerDamage - 1) / playerDamage
	bossTurns := (player.HitPoints + bossDamage - 1) / bossDamage

	return playerTurns <= bossTurns
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}