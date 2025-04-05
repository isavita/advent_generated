
#!/bin/bash

# --- Day 21: Monkey Math ---
# Part 1: Calculate the value yelled by the 'root' monkey.
# Uses awk for efficient parsing and evaluation with memoization.

INPUT_FILE="input.txt"

# Function to check if the input file exists
check_input_file() {
    if [[ ! -f "$INPUT_FILE" ]]; then
        echo "Error: Input file '$INPUT_FILE' not found." >&2
        exit 1
    fi
}

# Main logic implemented using awk
solve() {
    awk '
        # Function to evaluate a monkey recursively with memoization.
        # Local variables (v1, v2, op, parts, result) are declared in the parameter list
        # as per awk convention to keep them local to the function call.
        function evaluate(name,    v1, v2, op, parts, result) {
            # Check if value is already computed (memoization)
            if (name in values) {
                return values[name]
            }

            # If the monkey definition is not found in operations, it means
            # something is wrong (e.g., needed monkey not defined).
            # Number monkeys should already be in the `values` array from the main block.
            if (!(name in ops)) {
               print "Error: Definition not found for monkey: " name > "/dev/stderr"
               exit 1
            }

            # Parse the operation: "m1 op m2"
            split(ops[name], parts, " ") # parts[1]=m1, parts[2]=op, parts[3]=m2

            # Recursively evaluate the operands
            v1 = evaluate(parts[1])
            v2 = evaluate(parts[3])
            op = parts[2] # The operator

            # Perform the arithmetic operation
            if (op == "+") {
                result = v1 + v2
            } else if (op == "-") {
                result = v1 - v2
            } else if (op == "*") {
                result = v1 * v2
            } else if (op == "/") {
                # Check for division by zero
                if (v2 == 0) {
                     print "Error: Division by zero for monkey " name " (" v1 "/" v2 ")" > "/dev/stderr"
                     exit 1
                }
                # Use int() for integer division, as implied by the problem
                result = int(v1 / v2)
            } else {
                print "Error: Unknown operator '" op "' for monkey " name > "/dev/stderr"
                exit 1
            }

            # Store the computed value (memoization) and return it
            values[name] = result
            return result
        }

        # Main processing block: Executed for each line of the input file.
        # Parses the line and stores the definition in either the `values`
        # array (for numbers) or the `ops` array (for operations).
        {
            # Remove the colon from the monkey name for easier handling
            gsub(/:$/, "", $1)

            # Check the number of fields (NF) to determine the type of definition
            if (NF == 2) {
                # Line format: "name: number"
                values[$1] = $2  # Store number monkeys directly in values array
            } else if (NF == 4) {
                # Line format: "name: m1 op m2"
                # Store the operation string (m1 op m2) in the ops array
                ops[$1] = $2 " " $3 " " $4
            } else {
                 # Handle unexpected line format
                 print "Warning: Skipping malformed line: " $0 > "/dev/stderr"
            }
        }

        # END block: Executed after all lines from the input file have been processed.
        END {
            # Check if the root monkey definition exists either as a number or operation
            if (!("root" in values) && !("root" in ops)) {
                 print "Error: Monkey 'root' not defined in input file." > "/dev/stderr"
                 exit 1
            }
            # Start the evaluation process from the "root" monkey and print the result.
            # The evaluate function handles the recursive calculation and memoization.
            print evaluate("root")
        }
    ' "$INPUT_FILE"
}

# Main entry point of the script
main() {
    check_input_file
    solve
}

# Execute the main function
main
