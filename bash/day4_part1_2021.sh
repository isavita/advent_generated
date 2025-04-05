
#!/bin/bash

shopt -s extglob

declare -a all_board_numbers=()
declare -a all_marked_status=()
declare -a numbers_to_call=()

check_win() {
    local board_offset=$1
    local r c idx row_win col_win

    for (( r=0; r<5; r++ )); do
        row_win=1
        for (( c=0; c<5; c++ )); do
            idx=$(( board_offset + r*5 + c ))
            if [[ ${all_marked_status[$idx]} -eq 0 ]]; then
                row_win=0
                break
            fi
        done
        if [[ $row_win -eq 1 ]]; then return 0; fi
    done

    for (( c=0; c<5; c++ )); do
        col_win=1
        for (( r=0; r<5; r++ )); do
            idx=$(( board_offset + r*5 + c ))
            if [[ ${all_marked_status[$idx]} -eq 0 ]]; then
                col_win=0
                break
            fi
        done
        if [[ $col_win -eq 1 ]]; then return 0; fi
    done

    return 1
}

main() {
    local line
    IFS=, read -r -a numbers_to_call < <(head -n 1)

    local current_board_numbers=() i
    while read -r line; do
        if [[ -z "$line" ]]; then
            continue
        fi

        read -r -a row_nums <<< "$line"
        current_board_numbers+=("${row_nums[@]}")

        if [[ ${#current_board_numbers[@]} -eq 25 ]]; then
            all_board_numbers+=("${current_board_numbers[@]}")
            for (( i=0; i<25; i++ )); do
                all_marked_status+=(0)
            done
            current_board_numbers=()
        fi
    done < <(tail -n +2) # Read boards starting from the second line

    local num_boards=$((${#all_board_numbers[@]} / 25))
    local winning_board_idx=-1
    local winning_number=-1
    local called_num b board_offset idx found_in_board

    for called_num in "${numbers_to_call[@]}"; do
        for (( b=0; b<num_boards; b++ )); do
            board_offset=$(( b * 25 ))
            found_in_board=0

            for (( i=0; i<25; i++ )); do
                current_idx=$(( board_offset + i ))
                if [[ ${all_board_numbers[$current_idx]} -eq $called_num ]]; then
                    if [[ ${all_marked_status[$current_idx]} -eq 0 ]]; then
                       all_marked_status[$current_idx]=1
                       found_in_board=1
                    fi
                fi
            done

            if [[ $found_in_board -eq 1 ]]; then
                 if check_win "$board_offset"; then
                    winning_board_idx=$b
                    winning_number=$called_num
                    break 2
                fi
            fi
        done
    done

    if [[ $winning_board_idx -ne -1 ]]; then
        local unmarked_sum=0
        board_offset=$(( winning_board_idx * 25 ))
        for (( i=0; i<25; i++ )); do
            idx=$(( board_offset + i ))
            if [[ ${all_marked_status[$idx]} -eq 0 ]]; then
                unmarked_sum=$(( unmarked_sum + all_board_numbers[idx] ))
            fi
        done
        echo $(( unmarked_sum * winning_number ))
    fi
}

main < "input.txt"
