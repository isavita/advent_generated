
#!/bin/bash

main() {
    local H=6
    local W=50
    declare -a screen
    local r c i A B R C S shift_amount line
    local -a parts temp_col
    local initial_row char current_row_val target_row

    initial_row=$(printf '.%.0s' $(seq 1 $W))
    for (( r=0; r<H; r++ )); do
        screen[r]=$initial_row
    done

    while IFS= read -r line || [[ -n "$line" ]]; do
        read -ra parts <<< "$line"
        if [[ "${parts[0]}" == "rect" ]]; then
            A=${parts[1]%x*}
            B=${parts[1]#*x}
            for (( r=0; r<B && r<H; r++ )); do
                prefix="${screen[r]:0:A}"
                suffix="${screen[r]:A}"
                prefix=$(printf '#%.0s' $(seq 1 ${#prefix}))
                screen[r]="${prefix}${suffix}"
            done
        elif [[ "${parts[0]}" == "rotate" ]]; then
            S=${parts[4]}
            if [[ "${parts[1]}" == "row" ]]; then
                R=${parts[2]#*=}
                shift_amount=$(( S % W ))
                if [[ $shift_amount -ne 0 ]]; then
                    screen[R]="${screen[R]: W-shift_amount}${screen[R]:0:W-shift_amount}"
                fi
            elif [[ "${parts[1]}" == "column" ]]; then
                C=${parts[2]#*=}
                shift_amount=$(( S % H ))
                 if [[ $shift_amount -ne 0 ]]; then
                    declare -a temp_col
                    for (( i=0; i<H; i++ )); do
                        temp_col[i]=${screen[i]:C:1}
                    done
                    for (( i=0; i<H; i++ )); do
                        target_row=$(( (i + shift_amount) % H ))
                        char=${temp_col[i]}
                        current_row_val=${screen[target_row]}
                        screen[target_row]="${current_row_val:0:C}${char}${current_row_val:C+1}"
                    done
                fi
            fi
        fi
    done < "input.txt"

    for (( r=0; r<H; r++ )); do
        echo "${screen[r]}"
    done
}

main
