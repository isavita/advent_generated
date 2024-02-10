package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Ingredient struct {
	name       string
	capacity   int
	durability int
	flavor     int
	texture    int
	calories   int
}

func main() {
	ingredients, err := readIngredients("input.txt")
	if err != nil {
		fmt.Println("Error reading input:", err)
		return
	}

	maxScore := findMaxScore(ingredients, 100, 500)
	fmt.Println(maxScore)
}

func readIngredients(filename string) ([]Ingredient, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var ingredients []Ingredient
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		if len(parts) < 11 {
			continue // Invalid line
		}

		capacity, _ := strconv.Atoi(parts[2][:len(parts[2])-1])
		durability, _ := strconv.Atoi(parts[4][:len(parts[4])-1])
		flavor, _ := strconv.Atoi(parts[6][:len(parts[6])-1])
		texture, _ := strconv.Atoi(parts[8][:len(parts[8])-1])
		calories, _ := strconv.Atoi(parts[10])

		ingredients = append(ingredients, Ingredient{
			name:       parts[0],
			capacity:   capacity,
			durability: durability,
			flavor:     flavor,
			texture:    texture,
			calories:   calories,
		})
	}

	return ingredients, scanner.Err()
}

func findMaxScore(ingredients []Ingredient, totalTeaspoons, targetCalories int) int {
	return calculateMaxScore(ingredients, 0, totalTeaspoons, []int{}, targetCalories)
}

func calculateMaxScore(ingredients []Ingredient, index, remaining int, teaspoons []int, targetCalories int) int {
	if index == len(ingredients)-1 {
		teaspoons = append(teaspoons, remaining)
		if calculateCalories(ingredients, teaspoons) == targetCalories {
			return score(ingredients, teaspoons)
		}
		return 0
	}

	maxScore := 0
	for i := 0; i <= remaining; i++ {
		score := calculateMaxScore(ingredients, index+1, remaining-i, append(teaspoons, i), targetCalories)
		if score > maxScore {
			maxScore = score
		}
	}
	return maxScore
}

func score(ingredients []Ingredient, teaspoons []int) int {
	var capacity, durability, flavor, texture int
	for i, ingredient := range ingredients {
		capacity += ingredient.capacity * teaspoons[i]
		durability += ingredient.durability * teaspoons[i]
		flavor += ingredient.flavor * teaspoons[i]
		texture += ingredient.texture * teaspoons[i]
	}

	if capacity < 0 {
		capacity = 0
	}
	if durability < 0 {
		durability = 0
	}
	if flavor < 0 {
		flavor = 0
	}
	if texture < 0 {
		texture = 0
	}

	return capacity * durability * flavor * texture
}

func calculateCalories(ingredients []Ingredient, teaspoons []int) int {
	calories := 0
	for i, ingredient := range ingredients {
		calories += ingredient.calories * teaspoons[i]
	}
	return calories
}