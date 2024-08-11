#!/bin/bash

# Function to check if three sides can form a valid triangle
is_valid_triangle() {
    local a=$1 b=$2 c=$3
    (( a + b > c && a + c > b && b + c > a ))
}

# Part 1: Count valid triangles from rows
count_valid_triangles() {
    local count=0
    while read -r a b c; do
        if is_valid_triangle "$a" "$b" "$c"; then
            ((count++))
        fi
    done < input.txt
    echo "$count"
}

# Part 2: Count valid triangles from columns
count_valid_triangles_columns() {
    local count=0
    while read -r a1 a2 a3; do
        read -r b1 b2 b3
        read -r c1 c2 c3
        if is_valid_triangle "$a1" "$b1" "$c1"; then ((count++)); fi
        if is_valid_triangle "$a2" "$b2" "$c2"; then ((count++)); fi
        if is_valid_triangle "$a3" "$b3" "$c3"; then ((count++)); fi
    done < input.txt
    echo "$count"
}

# Output results
echo "Part 1: $(count_valid_triangles)"
echo "Part 2: $(count_valid_triangles_columns)"