
import java.util.Deque
import java.util.LinkedList

// Read input file
def lines = new File('input.txt').readLines()

// Extract parameters k, l, m
List<Integer> k = []
List<Integer> l = []
List<Integer> m = []

lines.eachWithIndex { line, i ->
    def parts = line.split()
    if (parts.size() < 3) return // Skip lines that don't have enough parts

    try {
        switch (i % 18) {
            case 4: // div z %d
                if (parts[0] == 'div' && parts[1] == 'z') {
                    l.add(Integer.parseInt(parts[2]))
                }
                break
            case 5: // add x %d
                if (parts[0] == 'add' && parts[1] == 'x') {
                    k.add(Integer.parseInt(parts[2]))
                }
                break
            case 15: // add y %d
                 if (parts[0] == 'add' && parts[1] == 'y') {
                    m.add(Integer.parseInt(parts[2]))
                }
                break
        }
    } catch (NumberFormatException e) {
        // Handle cases where parsing might fail, though not expected for valid input
        System.err.println("Warning: Could not parse number on line ${i + 1}: ${line}")
    }
}

// Determine constraints based on push (l=1) and pop (l=26) operations
Map<Integer, List<Integer>> constraints = [:]
Deque<Integer> stack = new LinkedList<>() // Use Deque for stack operations

l.eachWithIndex { val, i ->
    if (val == 1) {
        stack.push(i) // Push index
    } else if (val == 26) {
        if (stack.isEmpty()) {
             throw new IllegalStateException("Stack underflow at index ${i}")
        }
        int popIndex = stack.pop() // Pop index matching this push
        int diff = m[popIndex] + k[i]
        // Store constraint: push_index -> [pop_index, difference]
        // Where difference = digit[pop_index] - digit[push_index]
        constraints[popIndex] = [i, diff]
    }
}

// Calculate the digits for the maximum number satisfying constraints
int[] maxDigits = new int[14]

constraints.each { pushIndex, constraintData ->
    int popIndex = constraintData[0]
    int diff = constraintData[1] // diff = w[popIndex] - w[pushIndex]

    // To maximize the overall number, we want to maximize the earlier digit (pushIndex)
    // Constraint: 1 <= w[pushIndex] <= 9
    // Constraint: 1 <= w[popIndex] <= 9  => 1 <= w[pushIndex] + diff <= 9
    // Combine: max(1, 1 - diff) <= w[pushIndex] <= min(9, 9 - diff)
    // Maximize w[pushIndex] first:
    int wPush = Math.min(9, 9 - diff)
    int wPop = wPush + diff

    // Ensure wPush and wPop are valid digits (1-9)
    // The calculated wPush is the highest possible, ensuring wPop <= 9.
    // We just need to verify wPush >= 1 and wPop >= 1.
    // If this calculation leads to invalid digits (<1), the AoC input assumptions might be violated,
    // but typically the inputs are designed such that a valid solution exists following this logic.

    maxDigits[pushIndex] = wPush
    maxDigits[popIndex] = wPop
}

// Print the resulting maximum number
println maxDigits.join('')
