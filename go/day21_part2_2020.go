package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	allergenMap := make(map[string]map[string]bool)
	ingredientAllergen := make(map[string]string)
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, " (contains ")
		ingredients := strings.Fields(parts[0])
		var allergens []string
		if len(parts) > 1 {
			allergens = strings.Split(parts[1][:len(parts[1])-1], ", ")
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

	for len(allergenMap) > 0 {
		for allergen, ingredients := range allergenMap {
			if len(ingredients) == 1 {
				for ingredient := range ingredients {
					ingredientAllergen[allergen] = ingredient
					removeIngredientFromAll(allergenMap, ingredient)
				}
				delete(allergenMap, allergen)
			}
		}
	}

	allergens := make([]string, 0, len(ingredientAllergen))
	for allergen := range ingredientAllergen {
		allergens = append(allergens, allergen)
	}
	sort.Strings(allergens)

	result := make([]string, 0, len(allergens))
	for _, allergen := range allergens {
		result = append(result, ingredientAllergen[allergen])
	}

	fmt.Println(strings.Join(result, ","))
}

func contains(slice []string, str string) bool {
	for _, v := range slice {
		if v == str {
			return true
		}
	}
	return false
}

func removeIngredientFromAll(allergenMap map[string]map[string]bool, ingredient string) {
	for _, ingredients := range allergenMap {
		delete(ingredients, ingredient)
	}
}