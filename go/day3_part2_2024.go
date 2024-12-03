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
    re := regexp.MustCompile(`(mul\([0-9]{1,3},[0-9]{1,3}\))|(do\(\))|(don't\(\))`)

    // Find all matches and their indices
    matches := re.FindAllStringSubmatchIndex(input, -1)

    enabled := true // mul instructions are enabled at the beginning
    totalSum := 0

    for _, match := range matches {
        // match[0], match[1]: indices of the entire match
        // match[2], match[3]: indices of submatch 1 (mul)
        // match[4], match[5]: indices of submatch 2 (do)
        // match[6], match[7]: indices of submatch 3 (don't)

        if match[2] != -1 {
            // mul instruction matched
            if enabled {
                // Extract the substring
                mulStr := input[match[2]:match[3]]
                // Extract the numbers
                numsStr := mulStr[4 : len(mulStr)-1] // Remove "mul(" at start and ")" at end
                nums := strings.Split(numsStr, ",")
                if len(nums) != 2 {
                    continue
                }
                x, err1 := strconv.Atoi(nums[0])
                y, err2 := strconv.Atoi(nums[1])
                if err1 != nil || err2 != nil {
                    continue
                }
                totalSum += x * y
            }
        } else if match[4] != -1 {
            // do() instruction matched
            enabled = true
        } else if match[6] != -1 {
            // don't() instruction matched
            enabled = false
        }
    }

    fmt.Println(totalSum)
}
