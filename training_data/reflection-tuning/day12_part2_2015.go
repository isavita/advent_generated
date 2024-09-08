package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
)

func sumNumbers(data interface{}, ignoreRed bool) float64 {
	sum := 0.0

	switch v := data.(type) {
	case float64:
		return v
	case []interface{}:
		for _, item := range v {
			sum += sumNumbers(item, ignoreRed)
		}
	case map[string]interface{}:
		if ignoreRed {
			for _, value := range v {
				if str, ok := value.(string); ok && str == "red" {
					return 0
				}
			}
		}
		for _, value := range v {
			sum += sumNumbers(value, ignoreRed)
		}
	}

	return sum
}

func main() {
	data, err := ioutil.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	var jsonData interface{}
	err = json.Unmarshal(data, &jsonData)
	if err != nil {
		fmt.Println("Error parsing JSON:", err)
		return
	}

	// Part One
	sum1 := sumNumbers(jsonData, false)
	fmt.Printf("Part One: Sum of all numbers = %.0f\n", sum1)

	// Part Two
	sum2 := sumNumbers(jsonData, true)
	fmt.Printf("Part Two: Sum of numbers (ignoring red) = %.0f\n", sum2)
}
