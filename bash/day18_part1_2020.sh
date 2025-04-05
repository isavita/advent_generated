
#!/bin/bash

# Function for left-to-right evaluation (no parentheses)
evaluate_lr() {
    local expression="$1"
    local -a tokens
    read -ra tokens <<< "$expression"
    local result=${tokens[0]}
    local i=1
    while (( i < ${#tokens[@]} )); do
        local op="${tokens[i]}"
        local num="${tokens[i+1]}"
        case "$op" in
            '+') result=$(( result + num ));;
            '*') result=$(( result * num ));;
        esac
        ((i+=2))
    done
    echo "$result"
}

# Function to handle parentheses by iteratively evaluating innermost parts
evaluate() {
    local expression="$1"
    local prev_expression=""

    # Iteratively replace innermost parentheses until none are left
    while [[ "$expression" == *"("* ]] && [[ "$expression" != "$prev_expression" ]]; do
        prev_expression="$expression"
        # Find a simple parenthesis block (no nested parentheses within it)
        if [[ "$expression" =~ \(([^()]*)\) ]]; then
             local inner_expr="${BASH_REMATCH[1]}"
             local full_match_raw="${BASH_REMATCH[0]}"
             # Escape potential regex metacharacters in the match for safe replacement
             local full_match_escaped=$(sed 's/[()*+?.[\]\\^$]/\\&/g' <<< "$full_match_raw")

             local inner_result
             inner_result=$(evaluate_lr "$inner_expr")

             # Replace the first occurrence of this specific block
             expression="${expression/"$full_match_raw"/$inner_result}"
        else
             # Break if regex fails (e.g., malformed expression)
             break
        fi
    done

    # Evaluate the final expression (now without parentheses)
    evaluate_lr "$expression"
}

# Main execution block
main() {
    local total_sum=0
    local line
    local result

    if [[ ! -f "input.txt" ]]; then
        echo "Error: input.txt not found." >&2
        exit 1
    fi

    while IFS= read -r line || [[ -n "$line" ]]; do
        [[ -z "$line" ]] && continue # Skip empty lines
        result=$(evaluate "$line")
        total_sum=$(( total_sum + result ))
    done < "input.txt"

    echo "$total_sum"
}

# Run the main function
main
