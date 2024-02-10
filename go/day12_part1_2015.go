package main

import (
	"encoding/json"
	"fmt"
	"os"
)

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading input:", err)
		return
	}

	var jsonData interface{}
	if err := json.Unmarshal(data, &jsonData); err != nil {
		fmt.Println("Error parsing JSON:", err)
		return
	}

	sum := sumNumbers(jsonData)
	fmt.Println(sum)
}

func sumNumbers(data interface{}) int {
	sum := 0
	switch value := data.(type) {
	case []interface{}:
		for _, v := range value {
			sum += sumNumbers(v)
		}
	case map[string]interface{}:
		for _, v := range value {
			sum += sumNumbers(v)
		}
	case float64:
		sum += int(value)
	}
	return sum
}