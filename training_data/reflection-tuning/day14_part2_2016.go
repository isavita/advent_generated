package main

import (
	"bufio"
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"os"
	"strconv"
	"strings"
	"sync"
)

const (
	cacheSize  = 10000
	windowSize = 1000
	targetKeys = 64
)

func main() {
	salt := readInput("input.txt")
	fmt.Println("Part 1:", findNthKey(salt, 64, false))
	fmt.Println("Part 2:", findNthKey(salt, 64, true))
}

func readInput(filename string) string {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	return strings.TrimSpace(scanner.Text())
}

func findNthKey(salt string, n int, stretch bool) int {
	cache := make(map[int]string, cacheSize)
	var cacheMutex sync.Mutex
	keys := make([]int, 0, n)
	index := 0

	for len(keys) < n {
		hash := getHash(salt, index, stretch, &cache, &cacheMutex)
		if triplet := findTriplet(hash); triplet != "" {
			if checkQuintet(salt, index+1, index+windowSize, triplet, stretch, &cache, &cacheMutex) {
				keys = append(keys, index)
			}
		}
		index++
	}

	return keys[n-1]
}

func getHash(salt string, index int, stretch bool, cache *map[int]string, mutex *sync.Mutex) string {
	mutex.Lock()
	if hash, ok := (*cache)[index]; ok {
		mutex.Unlock()
		return hash
	}
	mutex.Unlock()

	hash := md5Hash(salt + strconv.Itoa(index))
	if stretch {
		for i := 0; i < 2016; i++ {
			hash = md5Hash(hash)
		}
	}

	mutex.Lock()
	if len(*cache) >= cacheSize {
		for k := range *cache {
			delete(*cache, k)
			break
		}
	}
	(*cache)[index] = hash
	mutex.Unlock()

	return hash
}

func md5Hash(input string) string {
	hash := md5.Sum([]byte(input))
	return hex.EncodeToString(hash[:])
}

func findTriplet(s string) string {
	for i := 0; i < len(s)-2; i++ {
		if s[i] == s[i+1] && s[i] == s[i+2] {
			return string(s[i])
		}
	}
	return ""
}

func checkQuintet(salt string, start, end int, char string, stretch bool, cache *map[int]string, mutex *sync.Mutex) bool {
	quintet := strings.Repeat(char, 5)
	var wg sync.WaitGroup
	result := make(chan bool, 1)

	for i := start; i <= end; i++ {
		wg.Add(1)
		go func(index int) {
			defer wg.Done()
			hash := getHash(salt, index, stretch, cache, mutex)
			if strings.Contains(hash, quintet) {
				select {
				case result <- true:
				default:
				}
			}
		}(i)
	}

	go func() {
		wg.Wait()
		close(result)
	}()

	return <-result
}
