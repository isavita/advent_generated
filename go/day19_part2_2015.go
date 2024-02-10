package main

import (
	"fmt"
	"math/rand"
	"os"
	"sort"
	"strings"
	"time"
)

func main() {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	input := strings.TrimSpace(string(file))
	fmt.Println(solve(input))
}

func parseInput(input string) (graph map[string][]string, startingMaterial []string) {
	blocks := strings.Split(input, "\n\n")
	startingMaterial = splitMolecules(blocks[1])

	graph = map[string][]string{}

	for _, l := range strings.Split(blocks[0], "\n") {
		parts := strings.Split(l, " => ")
		graph[parts[0]] = append(graph[parts[0]], parts[1])
	}

	return graph, startingMaterial
}

func splitMolecules(input string) []string {
	var molecules []string
	for _, char := range input {
		code := ToASCIICode(char)
		if code >= ASCIICodeCapA && code <= ASCIICodeCapZ {
			molecules = append(molecules, string(char))
		} else {
			molecules[len(molecules)-1] += string(char)
		}
	}
	return molecules
}

func solve(input string) int {
	reverseGraph, startingMols := parseInput(input)

	productToReactant := map[string]string{}
	for react, products := range reverseGraph {
		for _, p := range products {
			if _, ok := productToReactant[p]; ok {
				panic("dup found")
			}
			productToReactant[p] = react
		}
	}

	var allProducts []string
	for prod := range productToReactant {
		allProducts = append(allProducts, prod)
	}

	start := strings.Join(startingMols, "")
	mol := start

	var steps int
	for mol != "e" {
		var changeMade bool
		for _, prod := range allProducts {
			count := strings.Count(mol, prod)
			if count <= 0 {
				continue
			}
			changeMade = true
			steps += count
			mol = strings.ReplaceAll(mol, prod, productToReactant[prod])

			break
		}

		if !changeMade {
			allProducts = shuffleSlice(allProducts)
			mol = start
			steps = 0
		}
	}

	return steps
}

var rn = rand.New(rand.NewSource(time.Now().UnixNano()))

func shuffleSlice(in []string) []string {

	sort.Slice(in, func(i, j int) bool {
		return rn.Intn(2) == 1
	})
	return in
}

const (
	ASCIICodeCapA   = int('A')
	ASCIICodeCapZ   = int('Z')
	ASCIICodeLowerA = int('a')
	ASCIICodeLowerZ = int('z')
)

func ToASCIICode(arg interface{}) int {
	var asciiVal int
	switch arg.(type) {
	case string:
		str := arg.(string)
		if len(str) != 1 {
			panic("can only convert ascii Code for string of length 1")
		}
		asciiVal = int(str[0])
	case byte:
		asciiVal = int(arg.(byte))
	case rune:
		asciiVal = int(arg.(rune))
	}

	return asciiVal
}

func ASCIIIntToChar(code int) string {
	return string(rune(code))
}