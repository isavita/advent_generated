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

// Point represents a junction box in 3D space
type Point struct {
	id int
	x, y, z int
}

// Edge represents a potential connection between two junction boxes
type Edge struct {
	u, v   int // indices of the points
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

	if len(points) < 2 {
		fmt.Println("Not enough points to form circuits.")
		return
	}

	// 2. Generate all possible edges between pairs
	// For N points, there are N*(N-1)/2 edges.
	edges := make([]Edge, 0, len(points)*len(points)/2)
	for i := 0; i < len(points); i++ {
		for j := i + 1; j < len(points); j++ {
			d := distSq(points[i], points[j])
			edges = append(edges, Edge{u: i, v: j, distSq: d})
		}
	}

	// 3. Sort edges by distance (ascending)
	sort.Slice(edges, func(i, j int) bool {
		return edges[i].distSq < edges[j].distSq
	})

	// 4. Initialize Union-Find (Disjoint Set Union)
	parent := make([]int, len(points))
	size := make([]int, len(points))
	for i := 0; i < len(points); i++ {
		parent[i] = i
		size[i] = 1 // Each point starts as a circuit of size 1
	}

	// 5. Connect the top 1000 pairs
	limit := 1000
	if len(edges) < limit {
		limit = len(edges)
	}

	for i := 0; i < limit; i++ {
		union(parent, size, edges[i].u, edges[i].v)
	}

	// 6. Collect sizes of all distinct circuits
	var circuitSizes []int
	for i := 0; i < len(points); i++ {
		// If i is a root, add its size
		if parent[i] == i {
			circuitSizes = append(circuitSizes, size[i])
		}
	}

	// 7. Find the three largest circuits
	sort.Sort(sort.Reverse(sort.IntSlice(circuitSizes)))

	if len(circuitSizes) == 0 {
		fmt.Println("No circuits found.")
		return
	}

	// Calculate result
	result := 1
	count := 0
	for _, s := range circuitSizes {
		result *= s
		count++
		if count == 3 {
			break
		}
	}

	fmt.Printf("Product of three largest circuit sizes: %d\n", result)
}

// distSq calculates the squared Euclidean distance between two points
func distSq(p1, p2 Point) int {
	dx := p1.x - p2.x
	dy := p1.y - p2.y
	dz := p1.z - p2.z
	return dx*dx + dy*dy + dz*dz
}

// find finds the root of the set containing i with path compression
func find(parent []int, i int) int {
	if parent[i] != i {
		parent[i] = find(parent, parent[i])
	}
	return parent[i]
}

// union merges the sets containing i and j
func union(parent, size []int, i, j int) {
	rootI := find(parent, i)
	rootJ := find(parent, j)

	if rootI != rootJ {
		// Merge smaller set into larger set
		if size[rootI] < size[rootJ] {
			rootI, rootJ = rootJ, rootI
		}
		parent[rootJ] = rootI
		size[rootI] += size[rootJ]
	}
	// If rootI == rootJ, they are already in the same circuit; do nothing.
}
