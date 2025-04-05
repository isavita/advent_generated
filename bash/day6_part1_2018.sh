
#!/bin/bash

shopt -s extglob

main() {
    local input_file="input.txt"
    if [[ ! -f "$input_file" ]]; then
        echo "Error: Input file '$input_file' not found." >&2
        exit 1
    fi

    declare -a points_x
    declare -a points_y
    local i=0
    local x y px py
    local min_x=2147483647 max_x=-2147483648
    local min_y=2147483647 max_y=-2147483648

    while IFS=, read -r x y; do
        x=${x//[[:space:]]/}
        y=${y//[[:space:]]/}
        points_x[i]=$x
        points_y[i]=$y
        (( x < min_x )) && min_x=$x
        (( x > max_x )) && max_x=$x
        (( y < min_y )) && min_y=$y
        (( y > max_y )) && max_y=$y
        (( i++ ))
    done < "$input_file"
    local num_points=$i

    declare -A area_counts
    declare -A infinite_areas
    local dist min_dist closest_point_idx equal_dist count largest_area dx dy current_dist idx

    for (( i=0; i<num_points; i++ )); do
         area_counts[$i]=0
    done

    for (( x=min_x; x<=max_x; x++ )); do
        for (( y=min_y; y<=max_y; y++ )); do
            min_dist=2147483647
            closest_point_idx=-1
            equal_dist=0

            for (( i=0; i<num_points; i++ )); do
                px=${points_x[i]}
                py=${points_y[i]}

                (( dx = x - px ))
                (( dy = y - py ))
                (( dx < 0 )) && (( dx = -dx ))
                (( dy < 0 )) && (( dy = -dy ))
                (( current_dist = dx + dy ))

                if (( current_dist < min_dist )); then
                    min_dist=$current_dist
                    closest_point_idx=$i
                    equal_dist=0
                elif (( current_dist == min_dist )); then
                    equal_dist=1
                fi
            done

            if (( closest_point_idx != -1 && equal_dist == 0 )); then
                (( area_counts[$closest_point_idx]++ ))
                if (( x == min_x || x == max_x || y == min_y || y == max_y )); then
                    infinite_areas[$closest_point_idx]=1
                fi
            fi
        done
    done

    largest_area=0
    for idx in "${!area_counts[@]}"; do
        if [[ -z "${infinite_areas[$idx]}" ]]; then
            count=${area_counts[$idx]}
            if (( count > largest_area )); then
                largest_area=$count
            fi
        fi
    done

    echo "$largest_area"
}

main
