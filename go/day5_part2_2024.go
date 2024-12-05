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
        if !isCorrectlyOrdered(update, orderingRules) {
            sortedUpdate, err := sortUpdate(update, orderingRules)
            if err != nil {
                fmt.Println("Error sorting update:", err)
                continue
            }
            middlePage := sortedUpdate[len(sortedUpdate)/2]
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

func sortUpdate(update []int, rules [][]int) ([]int, error) {
    // Build graph
    adjacency := make(map[int][]int)
    pagesInUpdate := make(map[int]bool)
    for _, page := range update {
        pagesInUpdate[page] = true
        adjacency[page] = []int{} // initialize adjacency list
    }
    // Add edges
    for _, rule := range rules {
        x, y := rule[0], rule[1]
        if pagesInUpdate[x] && pagesInUpdate[y] {
            adjacency[x] = append(adjacency[x], y)
        }
    }
    // Perform topological sort
    visited := make(map[int]bool)
    tempMarked := make(map[int]bool) // for cycle detection
    result := []int{}
    var visit func(int) error
    visit = func(n int) error {
        if tempMarked[n] {
            return fmt.Errorf("Cycle detected")
        }
        if !visited[n] {
            tempMarked[n] = true
            for _, m := range adjacency[n] {
                if err := visit(m); err != nil {
                    return err
                }
            }
            tempMarked[n] = false
            visited[n] = true
            result = append(result, n)
        }
        return nil
    }
    for page := range pagesInUpdate {
        if !visited[page] {
            if err := visit(page); err != nil {
                return nil, err
            }
        }
    }
    // Reverse result to get correct order
    for i, j := 0, len(result)-1; i < j; i, j = i+1, j-1 {
        result[i], result[j] = result[j], result[i]
    }
    return result, nil
}
