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
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	allergenMap := make(map[string]map[string]bool)
	ingredientCount := make(map[string]int)
	safeIngredients := make(map[string]bool)

	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, " (contains ")
		ingredients := strings.Fields(parts[0])
		var allergens []string
		if len(parts) > 1 {
			allergens = strings.Split(parts[1][:len(parts[1])-1], ", ")
		}

		for _, ingredient := range ingredients {
			ingredientCount[ingredient]++
			safeIngredients[ingredient] = true
		}

		for _, allergen := range allergens {
			if _, exists := allergenMap[allergen]; !exists {
				allergenMap[allergen] = make(map[string]bool)
				for _, ingredient := range ingredients {
					allergenMap[allergen][ingredient] = true
				}
			} else {
				for ingredient := range allergenMap[allergen] {
					if !contains(ingredients, ingredient) {
						delete(allergenMap[allergen], ingredient)
					}
				}
			}
		}
	}

	for _, ingredients := range allergenMap {
		for ingredient := range ingredients {
			delete(safeIngredients, ingredient)
		}
	}

	count := 0
	for ingredient := range safeIngredients {
		count += ingredientCount[ingredient]
	}

	fmt.Println(count)
}

func contains(slice []string, str string) bool {
	for _, v := range slice {
		if v == str {
			return true
		}
	}
	return false
}