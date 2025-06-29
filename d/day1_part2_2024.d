
import std.stdio;
import std.string;
import std.conv;
import std.array;
import std.algorithm;
import std.math;
import std.range;

/**
 * This program solves the Day 1 "Historian Hysteria" challenge.
 * It reads pairs of numbers from input.txt and calculates two results:
 * 1. The sum of absolute differences between the two lists when both are sorted.
 * 2. A similarity score based on the frequency of numbers between the lists.
 */
void main() {
    // Use dynamic arrays to store the two lists of location IDs.
    // long is used for robustness with large numbers and sums.
    long[] leftList;
    long[] rightList;

    try {
        // Read all lines from the input file.
        foreach (line; File("input.txt").byLine) {
            // Split the line by whitespace and parse the two numbers.
            auto parts = line.strip.split;
            if (parts.length == 2) {
                leftList ~= to!long(parts[0]);
                rightList ~= to!long(parts[1]);
            }
        }
    } catch (Exception e) {
        stderr.writeln("Error: Could not read or parse input.txt.");
        stderr.writeln(e.msg);
        return;
    }

    // --- Part 1: Total Distance ---
    {
        // To preserve the original lists for Part 2, we work on duplicates.
        auto sortedLeft = leftList.dup;
        auto sortedRight = rightList.dup;

        // Sort both lists in-place.
        sortedLeft.sort;
        sortedRight.sort;

        // Use zip to pair elements from the sorted lists,
        // map to calculate the absolute difference for each pair,
        // and sum to get the final total. This is a concise, functional approach.
        long totalDistance = zip(sortedLeft, sortedRight)
                             .map!(pair => abs(pair[0] - pair[1]))
                             .sum;

        writeln("Part 1: The total distance is ", totalDistance);
    }

    // --- Part 2: Similarity Score ---
    {
        // Create a frequency map (associative array) for the right list.
        // This provides O(1) average time complexity for lookups.
        long[long] rightCounts;
        foreach (num; rightList) {
            rightCounts[num]++;
        }

        long similarityScore = 0;
        // Iterate through the original left list.
        foreach (leftNum; leftList) {
            // Safely get the count of leftNum from the right list's frequency map.
            // .get(key, defaultValue) returns 0 if the number isn't found.
            long count = rightCounts.get(leftNum, 0);
            similarityScore += leftNum * count;
        }

        writeln("Part 2: The similarity score is ", similarityScore);
    }
}
