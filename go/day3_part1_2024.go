package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "regexp"
    "strconv"
    "strings"
)

func main() {
    // Read the input file
    data, err := ioutil.ReadFile("input.txt")
    if err != nil {
        log.Fatal(err)
    }

    input := string(data)

    // Compile the regular expression
    re := regexp.MustCompile(`mul\([0-9]{1,3},[0-9]{1,3}\)`)

    // Find all matches
    matches := re.FindAllString(input, -1)

    totalSum := 0

    for _, match := range matches {
        // Remove 'mul(' at the beginning and ')' at the end
        params := strings.TrimSuffix(strings.TrimPrefix(match, "mul("), ")")
        // Split the parameters
        nums := strings.Split(params, ",")
        if len(nums) != 2 {
            continue
        }
        // Convert strings to integers
        x, err1 := strconv.Atoi(nums[0])
        y, err2 := strconv.Atoi(nums[1])
        if err1 != nil || err2 != nil {
            continue
        }
        // Multiply and add to total sum
        totalSum += x * y
    }

    // Print the total sum
    fmt.Println(totalSum)
}
