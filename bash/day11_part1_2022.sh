
#!/bin/bash

# Declare arrays to hold monkey properties
declare -a items op_operator op_operand div test_true test_false inspections

# --- Parsing Logic ---
parse_input() {
    local monkey_index=-1
    local line

    while IFS= read -r line || [[ -n "$line" ]]; do
        # Trim whitespace
        line=$(echo "$line" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

        if [[ "$line" =~ ^Monkey ]]; then
            ((monkey_index++))
            inspections[monkey_index]=0
        elif [[ "$line" =~ ^Starting\ items:\ (.*) ]]; then
            local items_str=${BASH_REMATCH[1]}
            # Store items as space-separated string, removing commas
            items[monkey_index]=${items_str//,/}
        elif [[ "$line" =~ ^Operation:\ new\ =\ old\ ([\+\*])\ (.*) ]]; then
            op_operator[monkey_index]=${BASH_REMATCH[1]}
            op_operand[monkey_index]=${BASH_REMATCH[2]}
        elif [[ "$line" =~ ^Test:\ divisible\ by\ ([0-9]+) ]]; then
            div[monkey_index]=${BASH_REMATCH[1]}
        elif [[ "$line" =~ ^If\ true:\ throw\ to\ monkey\ ([0-9]+) ]]; then
            test_true[monkey_index]=${BASH_REMATCH[1]}
        elif [[ "$line" =~ ^If\ false:\ throw\ to\ monkey\ ([0-9]+) ]]; then
            test_false[monkey_index]=${BASH_REMATCH[1]}
        fi
    done
}

# --- Simulation Logic ---
simulate() {
    local rounds=$1
    local num_monkeys=${#items[@]}
    local round m item worry_level new_worry val op operand target_monkey test_val

    for ((round = 0; round < rounds; round++)); do
        for ((m = 0; m < num_monkeys; m++)); do
            # Process items for monkey m using a while loop, modifying the string
            while [[ -n "${items[m]}" ]]; do
                # Get first item
                item=${items[m]%% *}
                # Remove first item (handle single item case)
                if [[ "${items[m]}" == "$item" ]]; then
                    items[m]="" # Last item
                else
                    items[m]=${items[m]#* } # Remove first item and space
                fi

                ((inspections[m]++))

                # Perform operation
                op=${op_operator[m]}
                operand=${op_operand[m]}
                worry_level=$item

                if [[ "$operand" == "old" ]]; then
                    val=$worry_level
                else
                    val=$operand
                fi

                if [[ "$op" == "+" ]]; then
                    ((new_worry = worry_level + val))
                else # Assume "*"
                    ((new_worry = worry_level * val))
                fi

                # Part 1: Worry level decreases
                ((new_worry /= 3))

                # Test divisibility
                test_val=${div[m]}
                if ((new_worry % test_val == 0)); then
                    target_monkey=${test_true[m]}
                else
                    target_monkey=${test_false[m]}
                fi

                # Throw item: Append to target monkey's item list
                if [[ -z "${items[target_monkey]}" ]]; then
                    items[target_monkey]="$new_worry"
                else
                    items[target_monkey]="${items[target_monkey]} $new_worry"
                fi
            done
        done
    done
}

# --- Result Calculation ---
calculate_result() {
    local top1 top2
    # Sort inspections numerically descending, get top 2, calculate product
    printf "%s\n" "${inspections[@]}" | sort -nr | head -n 2 | {
        read top1
        read top2
        echo $((top1 * top2))
    }
}

# --- Main Entry Point ---
main() {
    if [[ ! -f "input.txt" ]]; then
        echo "Error: input.txt not found." >&2
        exit 1
    fi
    parse_input < "input.txt"
    simulate 20
    calculate_result
}

# --- Run Main ---
main
