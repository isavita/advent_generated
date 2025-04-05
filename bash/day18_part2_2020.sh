
#!/bin/bash

shopt -s extglob

# Function for Part 1 evaluation (no parentheses) - Left-to-Right
eval_simple_no_paren() {
    awk '{
        val = $1;
        for (i = 2; i <= NF; i += 2) {
            op = $i;
            num = $(i+1);
            if (op == "+") { val += num; }
            else if (op == "*") { val *= num; }
        }
        print val;
    }' <<< "$1"
}

# Function for Part 2 evaluation (no parentheses) - Addition First
eval_advanced_no_paren() {
    local current_expr="$1"
    # Handle additions first using Bash regex substitution loop
    while true; do
         if [[ "$current_expr" =~ ([0-9]+)[[:space:]]*\+[[:space:]]*([0-9]+) ]]; then
            local lhs="${BASH_REMATCH[1]}"
            local rhs="${BASH_REMATCH[2]}"
            local sum=$((lhs + rhs))
            local match_str="${BASH_REMATCH[0]}"
            # Replace first occurrence using parameter expansion
            current_expr="${current_expr/"$match_str"/$sum}"
        else
            break # No more additions
        fi
    done
    # Evaluate remaining (multiplication only) expression with bc
    # bc needs spaces converted to '*' if not already there, but standard input should be fine.
    echo "$current_expr" | bc
}

# Evaluates expression with parentheses, calling the appropriate no-paren evaluator
evaluate_expression() {
    local current_expr="$1"
    local eval_func_name="$2"

    while [[ "$current_expr" == *\(* ]]; do
        # Find the innermost expression: from the last '(' up to the first ')' after it
        local prefix="${current_expr%\(*}"
        local suffix="${current_expr#"$prefix"}" # Starts with '('
        suffix="${suffix#\(}" # Remove leading '('
        local inner_content="${suffix%%\)*}" # Content up to the first ')'
        local rest_of_suffix="${suffix#"$inner_content"}" # Starts with ')'
        rest_of_suffix="${rest_of_suffix#\)}" # Remove leading ')'

        # Evaluate the inner content using the specified function (simple or advanced)
        local inner_result
        inner_result=$("$eval_func_name" "$inner_content")

        # Substitute the evaluated result back into the expression
        current_expr="${prefix}${inner_result}${rest_of_suffix}"
    done

    # Evaluate the final expression (no parentheses left)
    "$eval_func_name" "$current_expr"
}

main() {
    local total1=0
    local total2=0
    local line

    if [[ ! -f "input.txt" ]]; then
        echo "Error: input.txt not found." >&2
        exit 1
    fi

    if ! command -v bc &> /dev/null || ! command -v awk &> /dev/null; then
         echo "Error: bc and awk commands must be available." >&2
         exit 1
    fi


    while IFS= read -r line || [[ -n "$line" ]]; do
        # Clean up extra spaces just in case
        line=$(echo "$line" | tr -s ' ' | awk '{$1=$1; print}')
        [[ -z "$line" ]] && continue # Skip empty lines

        result1=$(evaluate_expression "$line" "eval_simple_no_paren")
        result2=$(evaluate_expression "$line" "eval_advanced_no_paren")

        total1=$((total1 + result1))
        total2=$((total2 + result2))
    done < "input.txt"

    echo "$total1"
    echo "$total2"
}

main
