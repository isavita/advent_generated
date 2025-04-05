
#!/bin/bash

shopt -s expand_aliases
declare -A grid
declare -A row_min_x row_max_x col_min_y col_max_y
declare -a instructions
declare -a map_lines

read_input() {
    local map_section=1
    local line_num=0
    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ $map_section -eq 1 ]]; then
            if [[ -z "$line" ]]; then
                map_section=0
            else
                map_lines+=("$line")
                ((line_num++))
            fi
        else
            path_str="$line"
            break 
        fi
    done < "input.txt"
}

parse_map() {
    num_rows=${#map_lines[@]}
    max_width=0
    for line in "${map_lines[@]}"; do
        if [[ ${#line} -gt $max_width ]]; then
            max_width=${#line}
        fi
    done
    num_cols=$max_width

    for (( y=0; y<num_rows; y++ )); do
        local padded_line
        printf -v padded_line "%-*s" "$max_width" "${map_lines[$y]}"
        grid[$y]="$padded_line"
    done

    for (( y=0; y<num_rows; y++ )); do
        local min_x=-1
        local max_x=-1
        local row="${grid[$y]}"
        for (( x=0; x<max_width; x++ )); do
            if [[ "${row:$x:1}" != " " ]]; then
                if [[ $min_x -eq -1 ]]; then
                    min_x=$x
                fi
                max_x=$x
            fi
        done
        row_min_x[$y]=$min_x
        row_max_x[$y]=$max_x
    done

    for (( x=0; x<max_width; x++ )); do
        local min_y=-1
        local max_y=-1
        for (( y=0; y<num_rows; y++ )); do
             local row="${grid[$y]}"
             if [[ $x -lt ${#row} && "${row:$x:1}" != " " ]]; then
                 if [[ $min_y -eq -1 ]]; then
                    min_y=$y
                 fi
                 max_y=$y
             fi
        done
         col_min_y[$x]=$min_y
         col_max_y[$x]=$max_y
    done
}

parse_path() {
    local pattern='[0-9]+|[LR]'
    while IFS= read -r match; do
        instructions+=("$match")
    done < <(grep -oE "$pattern" <<< "$path_str")
}

find_start() {
    local start_y=0
    local start_x=${row_min_x[0]}
    echo "$start_x $start_y"
}

simulate() {
    read -r curr_x curr_y < <(find_start)
    local facing=0 # 0:R, 1:D, 2:L, 3:U
    local dx=(1 0 -1 0)
    local dy=(0 1 0 -1)

    for instr in "${instructions[@]}"; do
        if [[ "$instr" =~ ^[0-9]+$ ]]; then
            local steps=$instr
            for (( s=0; s<steps; s++ )); do
                local next_x=$((curr_x + dx[facing]))
                local next_y=$((curr_y + dy[facing]))

                # Wrap around
                if [[ $facing -eq 0 ]]; then # Right
                    if [[ $next_x -gt ${row_max_x[$curr_y]} ]]; then next_x=${row_min_x[$curr_y]}; fi
                elif [[ $facing -eq 2 ]]; then # Left
                    if [[ $next_x -lt ${row_min_x[$curr_y]} ]]; then next_x=${row_max_x[$curr_y]}; fi
                elif [[ $facing -eq 1 ]]; then # Down
                     if [[ $next_y -gt ${col_max_y[$curr_x]} ]]; then next_y=${col_min_y[$curr_x]}; fi
                elif [[ $facing -eq 3 ]]; then # Up
                     if [[ $next_y -lt ${col_min_y[$curr_x]} ]]; then next_y=${col_max_y[$curr_x]}; fi
                fi

                local tile_row="${grid[$next_y]}"
                local tile="${tile_row:$next_x:1}"

                if [[ "$tile" == "#" ]]; then
                    break # Hit wall
                elif [[ "$tile" == "." ]]; then
                    curr_x=$next_x
                    curr_y=$next_y
                fi
            done
        elif [[ "$instr" == "R" ]]; then
            facing=$(((facing + 1) % 4))
        elif [[ "$instr" == "L" ]]; then
            facing=$(((facing + 3) % 4)) # equivalent to (facing - 1 + 4) % 4
        fi
    done
    echo "$curr_x $curr_y $facing"
}

main() {
    read_input
    parse_map
    parse_path
    read -r final_x final_y final_facing < <(simulate)
    
    # Adjust 0-based index to 1-based for password calculation
    local password=$(( 1000 * (final_y + 1) + 4 * (final_x + 1) + final_facing ))
    echo "$password"
}

main
