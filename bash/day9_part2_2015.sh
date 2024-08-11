#!/bin/bash

# Read the input file
input_file="input.txt"

# Check if the input file exists
if [[ ! -f "$input_file" ]]; then
    echo "Input file not found!"
    exit 1
fi

# Read the input file into a variable
input_data=$(<"$input_file")

# Use Python to compute the shortest and longest routes
python3 - <<EOF
import sys
from itertools import permutations

# Parse the input data
distances = {}
for line in """$input_data""".strip().split('\n'):
    parts = line.split(' = ')
    cities = parts[0].split(' to ')
    distance = int(parts[1])
    city1, city2 = cities[0], cities[1]
    if city1 not in distances:
        distances[city1] = {}
    if city2 not in distances:
        distances[city2] = {}
    distances[city1][city2] = distance
    distances[city2][city1] = distance

# Get the list of cities
cities = list(distances.keys())

# Generate all permutations of cities
all_routes = permutations(cities)

# Initialize variables to track the shortest and longest routes
shortest_route = float('inf')
longest_route = 0

# Calculate the distance for each route
for route in all_routes:
    distance = 0
    for i in range(len(route) - 1):
        city1 = route[i]
        city2 = route[i + 1]
        distance += distances[city1][city2]
    shortest_route = min(shortest_route, distance)
    longest_route = max(longest_route, distance)

# Print the results
print("Shortest route distance:", shortest_route)
print("Longest route distance:", longest_route)
EOF