package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
)

func main() {
	input, _ := ioutil.ReadFile("input.txt")
	numRecipes, _ := strconv.Atoi(string(input))

	recipes := []int{3, 7}
	elf1, elf2 := 0, 1

	for len(recipes) < numRecipes+10 {
		sum := recipes[elf1] + recipes[elf2]
		if sum >= 10 {
			recipes = append(recipes, sum/10)
		}
		recipes = append(recipes, sum%10)

		elf1 = (elf1 + 1 + recipes[elf1]) % len(recipes)
		elf2 = (elf2 + 1 + recipes[elf2]) % len(recipes)
	}

	result := ""
	for i := numRecipes; i < numRecipes+10; i++ {
		result += strconv.Itoa(recipes[i])
	}

	fmt.Println(result)
}
