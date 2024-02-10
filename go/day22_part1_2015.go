package main

import (
	"bufio"
	"os"
	"strconv"
	"strings"
)

type GameState struct {
	playerHP, playerMana, bossHP, bossDamage int
	shieldTimer, poisonTimer, rechargeTimer  int
	manaSpent                                int
}

func minManaToWin(initialState GameState) int {
	minMana := int(^uint(0) >> 1) // Max int value
	var simulate func(state GameState, playerTurn bool)
	simulate = func(state GameState, playerTurn bool) {
		if state.manaSpent >= minMana {
			return
		}
		if state.bossHP <= 0 {
			minMana = state.manaSpent
			return
		}
		if state.playerHP <= 0 {
			return
		}

		// Apply effects
		if state.shieldTimer > 0 {
			state.shieldTimer--
		}
		if state.poisonTimer > 0 {
			state.bossHP -= 3
			state.poisonTimer--
		}
		if state.rechargeTimer > 0 {
			state.playerMana += 101
			state.rechargeTimer--
		}

		if !playerTurn {
			damage := state.bossDamage
			if state.shieldTimer > 0 {
				damage -= 7
			}
			if damage < 1 {
				damage = 1
			}
			state.playerHP -= damage
			simulate(state, true)
			return
		}

		if state.playerMana >= 53 {
			newState := state
			newState.playerMana -= 53
			newState.manaSpent += 53
			newState.bossHP -= 4
			simulate(newState, false)
		}
		if state.playerMana >= 73 {
			newState := state
			newState.playerMana -= 73
			newState.manaSpent += 73
			newState.bossHP -= 2
			newState.playerHP += 2
			simulate(newState, false)
		}
		if state.playerMana >= 113 && state.shieldTimer == 0 {
			newState := state
			newState.playerMana -= 113
			newState.manaSpent += 113
			newState.shieldTimer = 6
			simulate(newState, false)
		}
		if state.playerMana >= 173 && state.poisonTimer == 0 {
			newState := state
			newState.playerMana -= 173
			newState.manaSpent += 173
			newState.poisonTimer = 6
			simulate(newState, false)
		}
		if state.playerMana >= 229 && state.rechargeTimer == 0 {
			newState := state
			newState.playerMana -= 229
			newState.manaSpent += 229
			newState.rechargeTimer = 5
			simulate(newState, false)
		}
	}

	initialState.playerHP = 50
	initialState.playerMana = 500
	simulate(initialState, true)
	return minMana
}

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	bossHP, _ := strconv.Atoi(strings.Split(scanner.Text(), ": ")[1])
	scanner.Scan()
	bossDamage, _ := strconv.Atoi(strings.Split(scanner.Text(), ": ")[1])

	initialState := GameState{bossHP: bossHP, bossDamage: bossDamage}
	println(minManaToWin(initialState))
}