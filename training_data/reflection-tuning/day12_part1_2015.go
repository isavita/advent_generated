package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
)

func sumNumbers(data interface{}) float64 {
	var sum float64

	switch v := data.(type) {
	case float64:
		return v
	case []interface{}:
		for _, item := range v {
			sum += sumNumbers(item)
		}
	case map[string]interface{}:
		for _, value := range v {
			sum += sumNumbers(value)
		}
	}

	return sum
}

func main() {
	// Read input file
	content, err := ioutil.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	// Parse JSON
	var data interface{}
	err = json.Unmarshal(content, &data)
	if err != nil {
		fmt.Println("Error parsing JSON:", err)
		return
	}

	// Calculate sum
	result := sumNumbers(data)

	// Print result
	fmt.Printf("Sum of all numbers: %.0f\n", result)
}
