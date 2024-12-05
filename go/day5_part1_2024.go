package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

func main() {
    orderingRules, updates, err := readInput("input.txt")
    if err != nil {
        fmt.Println("Error reading input:", err)
        return
    }

    sum := 0
    for _, update := range updates {
        if isCorrectlyOrdered(update, orderingRules) {
            middlePage := update[len(update)/2]
            sum += middlePage
        }
    }

    fmt.Println(sum)
}

func readInput(filename string) ([][]int, [][]int, error) {
    file, err := os.Open(filename)
    if err != nil {
        return nil, nil, err
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    orderingRules := [][]int{}
    updates := [][]int{}
    isUpdateSection := false

    for scanner.Scan() {
        line := scanner.Text()
        line = strings.TrimSpace(line)
        if line == "" {
            isUpdateSection = true
            continue
        }

        if !isUpdateSection {
            parts := strings.Split(line, "|")
            if len(parts) != 2 {
                continue
            }
            x, err1 := strconv.Atoi(strings.TrimSpace(parts[0]))
            y, err2 := strconv.Atoi(strings.TrimSpace(parts[1]))
            if err1 != nil || err2 != nil {
                continue
            }
            orderingRules = append(orderingRules, []int{x, y})
        } else {
            nums := strings.Split(line, ",")
            update := []int{}
            for _, numStr := range nums {
                numStr = strings.TrimSpace(numStr)
                num, err := strconv.Atoi(numStr)
                if err != nil {
                    continue
                }
                update = append(update, num)
            }
            if len(update) > 0 {
                updates = append(updates, update)
            }
        }
    }

    if err := scanner.Err(); err != nil {
        return nil, nil, err
    }

    return orderingRules, updates, nil
}

func isCorrectlyOrdered(update []int, rules [][]int) bool {
    position := make(map[int]int)
    for idx, page := range update {
        position[page] = idx
    }

    for _, rule := range rules {
        x, y := rule[0], rule[1]
        posX, okX := position[x]
        posY, okY := position[y]
        if okX && okY {
            if posX >= posY {
                return false
            }
        }
    }

    return true
}
