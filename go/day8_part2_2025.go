package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

// Point represents a junction box
type Point struct {
	id      int
	x, y, z int
}

// Edge represents a possible connection
type Edge struct {
	u, v   int // indices in the points slice
	distSq int // squared Euclidean distance
}

func main() {
	// 1. Read Input
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var points []Point
	scanner := bufio.NewScanner(file)
	idCounter := 0

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		parts := strings.Split(line, ",")
		if len(parts) != 3 {
			log.Fatalf("Invalid line format: %s", line)
		}

		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		z, _ := strconv.Atoi(parts[2])

		points = append(points, Point{id: idCounter, x: x, y: y, z: z})
		idCounter++
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	n := len(points)
	if n < 2 {
		fmt.Println("Not enough points to form a circuit.")
		return
	}

	// 2. Generate all edges
	edges := make([]Edge, 0, n*(n-1)/2)
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			d := distSq(points[i], points[j])
			edges = append(edges, Edge{u: i, v: j, distSq: d})
		}
	}

	// 3. Sort edges by distance (ascending)
	sort.Slice(edges, func(i, j int) bool {
		return edges[i].distSq < edges[j].distSq
	})

	// 4. Initialize Union-Find
	parent := make([]int, n)
	for i := 0; i < n; i++ {
		parent[i] = i
	}

	// We start with N disjoint components.
	components := n

	// 5. Iterate through edges
	for _, edge := range edges {
		rootU := find(parent, edge.u)
		rootV := find(parent, edge.v)

		// If they are in different sets, connect them
		if rootU != rootV {
			// Union logic
			parent[rootV] = rootU
			components--

			// Check if we just formed a single connected component
			if components == 1 {
				p1 := points[edge.u]
				p2 := points[edge.v]
				
				fmt.Printf("Connected %d,%d,%d and %d,%d,%d\n", p1.x, p1.y, p1.z, p2.x, p2.y, p2.z)
				
				// Calculate result: Product of X coordinates
				result := p1.x * p2.x
				fmt.Printf("Product of X coordinates: %d\n", result)
				return
			}
		}
	}
}

// distSq calculates squared Euclidean distance
func distSq(p1, p2 Point) int {
	dx := p1.x - p2.x
	dy := p1.y - p2.y
	dz := p1.z - p2.z
	return dx*dx + dy*dy + dz*dz
}

// find with path compression
func find(parent []int, i int) int {
	if parent[i] != i {
		parent[i] = find(parent, parent[i])
	}
	return parent[i]
}
