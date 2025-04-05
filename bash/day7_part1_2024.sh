
#!/bin/bash

_evaluate_recursive() {
    local current_val="$1"
    local target="$2"
    shift 2
    local -a remaining_nums=("$@")
    local next_num
    local -a next_remaining
    local val_add
    local val_mul

    if [[ ${#remaining_nums[@]} -eq 0 ]]; then
        if (( current_val == target )); then return 0; else return 1; fi
    fi

    next_num="${remaining_nums[0]}"
    next_remaining=("${remaining_nums[@]:1}")

    val_add=$(( current_val + next_num ))
    if _evaluate_recursive "$val_add" "$target" "${next_remaining[@]}"; then
        return 0
    fi

    val_mul=$(( current_val * next_num ))
    if _evaluate_recursive "$val_mul" "$target" "${next_remaining[@]}"; then
        return 0
    fi

    return 1
}

evaluate_expression() {
    local target="$1"
    shift
    local -a nums=("$@")
    local first_num
    local -a rest_nums

    if [[ ${#nums[@]} -eq 0 ]]; then return 1; fi

    first_num="${nums[0]}"
    rest_nums=("${nums[@]:1}")

    if [[ ${#rest_nums[@]} -eq 0 ]]; then
        if (( first_num == target )); then return 0; else return 1; fi
    fi

    _evaluate_recursive "$first_num" "$target" "${rest_nums[@]}"
}

main() {
    local total_sum=0
    local target_value
    local numbers_str
    local -a numbers

    if [[ ! -f "input.txt" ]]; then
        echo "Error: input.txt not found." >&2
        exit 1
    fi

    while IFS=': ' read -r target_value numbers_str || [[ -n "$target_value" ]]; do
         # Handle lines potentially ending without newline
        if [[ -z "$target_value" ]]; then continue; fi
        read -r -a numbers <<< "$numbers_str"
        if (( ${#numbers[@]} == 0 )); then continue; fi

        if evaluate_expression "$target_value" "${numbers[@]}"; then
            total_sum=$((total_sum + target_value))
        fi
    done < "input.txt"

    echo "$total_sum"
}

main "$@"
