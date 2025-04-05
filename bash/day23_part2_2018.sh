
#!/bin/bash

shopt -s extglob # Enable extended globbing for pattern matching

# --- Global Variables ---
declare -a bots_x bots_y bots_z bots_r
num_bots=0

# --- Helper Functions ---
abs_val() {
    local n=$1
    if (( n < 0 )); then
        echo $(( -n ))
    else
        echo "$n"
    fi
}

manhattan_distance() {
    local x1=$1 y1=$2 z1=$3 x2=$4 y2=$5 z2=$6
    local dx=$(( x1 - x2 ))
    local dy=$(( y1 - y2 ))
    local dz=$(( z1 - z2 ))
    local abs_dx=$(abs_val "$dx")
    local abs_dy=$(abs_val "$dy")
    local abs_dz=$(abs_val "$dz")
    echo $(( abs_dx + abs_dy + abs_dz ))
}

min_distance_to_origin() {
    local x=$1 y=$2 z=$3 size=$4
    local x_max=$(( x + size - 1 ))
    local y_max=$(( y + size - 1 ))
    local z_max=$(( z + size - 1 ))
    local dx=0 dy=0 dz=0

    if (( x > 0 )); then dx=$x
    elif (( x_max < 0 )); then dx=$(abs_val "$x_max"); fi

    if (( y > 0 )); then dy=$y
    elif (( y_max < 0 )); then dy=$(abs_val "$y_max"); fi

    if (( z > 0 )); then dz=$z
    elif (( z_max < 0 )); then dz=$(abs_val "$z_max"); fi

    echo $(( dx + dy + dz ))
}

# --- Input Parsing ---
parse_input() {
    local file_path="$1"
    local i=0
    local pattern='pos=<(-?[0-9]+),(-?[0-9]+),(-?[0-9]+)>, r=([0-9]+)'
    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ "$line" =~ $pattern ]]; then
            bots_x[i]=${BASH_REMATCH[1]}
            bots_y[i]=${BASH_REMATCH[2]}
            bots_z[i]=${BASH_REMATCH[3]}
            bots_r[i]=${BASH_REMATCH[4]}
            ((i++))
        fi
    done < "$file_path"
    num_bots=$i
}

# --- Part One ---
part_one() {
    local max_r=-1
    local strongest_idx=-1
    for (( i=0; i<num_bots; i++ )); do
        if (( bots_r[i] > max_r )); then
            max_r=${bots_r[i]}
            strongest_idx=$i
        fi
    done

    if (( strongest_idx == -1 )); then
        echo 0
        return
    fi

    local sx=${bots_x[strongest_idx]}
    local sy=${bots_y[strongest_idx]}
    local sz=${bots_z[strongest_idx]}
    local sr=${bots_r[strongest_idx]}

    local count=0
    for (( i=0; i<num_bots; i++ )); do
        local dist=$(manhattan_distance "$sx" "$sy" "$sz" "${bots_x[i]}" "${bots_y[i]}" "${bots_z[i]}")
        if (( dist <= sr )); then
            ((count++))
        fi
    done
    echo "$count"
}

# --- Part Two ---
# Note: Part Two uses a priority queue (heap) and spatial partitioning (octree).
# Simulating this efficiently in pure Bash is extremely challenging and slow.
# This implementation uses `sort` to simulate the priority queue, which will be
# very inefficient for large inputs.
part_two() {
    local min_x=${bots_x[0]} max_x=${bots_x[0]}
    local min_y=${bots_y[0]} max_y=${bots_y[0]}
    local min_z=${bots_z[0]} max_z=${bots_z[0]}

    for (( i=1; i<num_bots; i++ )); do
        (( bots_x[i] < min_x )) && min_x=${bots_x[i]}
        (( bots_x[i] > max_x )) && max_x=${bots_x[i]}
        (( bots_y[i] < min_y )) && min_y=${bots_y[i]}
        (( bots_y[i] > max_y )) && max_y=${bots_y[i]}
        (( bots_z[i] < min_z )) && min_z=${bots_z[i]}
        (( bots_z[i] > max_z )) && max_z=${bots_z[i]}
    done

    local size=1
    local max_dim_diff=0
    local dx=$(( max_x - min_x ))
    local dy=$(( max_y - min_y ))
    local dz=$(( max_z - min_z ))
    (( dx > max_dim_diff )) && max_dim_diff=$dx
    (( dy > max_dim_diff )) && max_dim_diff=$dy
    (( dz > max_dim_diff )) && max_dim_diff=$dz

    while (( size < max_dim_diff )); do
        size=$(( size * 2 ))
    done

    # Use a temporary file to simulate the heap
    local heap_file
    heap_file=$(mktemp)
    trap 'rm -f "$heap_file"' EXIT # Ensure cleanup

    # Initial cube: count=0 (bots covering it), dist, size, x, y, z
    local initial_dist=$(min_distance_to_origin "$min_x" "$min_y" "$min_z" "$size")
    # Format: neg_count distance size x y z (neg_count for max-heap behavior via min-sort)
    echo "0 $initial_dist $size $min_x $min_y $min_z" > "$heap_file"

    local best_distance=-1
    local best_count=-1

    while true; do
        # Sort the file to get the highest priority element at the top
        # Sort by neg_count (ascending, so most bots first), then distance (ascending)
        sort -k1,1n -k2,2n -o "$heap_file" "$heap_file"

        # Check if heap is empty
        if [[ ! -s "$heap_file" ]]; then
             break
        fi

        # Pop the top element (read and remove)
        local top_line
        read -r top_line < "$heap_file"
        # Use awk for safer "remove first line" than sed -i
        awk 'NR > 1' "$heap_file" > "$heap_file.tmp" && mv "$heap_file.tmp" "$heap_file"

        local neg_count current_dist current_size x y z
        read -r neg_count current_dist current_size x y z <<< "$top_line"
        local current_count=$(( -neg_count ))

        # Pruning based on already found best (optimization)
        # If we can't possibly beat the best count found so far, prune this branch.
        # Note: This specific pruning wasn't explicitly in the python, but is common.
        if (( best_count != -1 && current_count < best_count )); then
             continue
        fi

        if (( current_size == 1 )); then
            # We've reached a single point (voxel)
            # This is the best point found *so far* according to the priority
            # Since we process highest count / lowest distance first, this is the optimal one.
             best_count=$current_count
             best_distance=$current_dist
             break # Found the optimal point
        fi

        # Split the cube into 8 sub-cubes
        local half=$(( current_size / 2 ))
        [[ $half -lt 1 ]] && half=1 # Ensure size doesn't go below 1

        for dx_off in 0 "$half"; do
            for dy_off in 0 "$half"; do
                for dz_off in 0 "$half"; do
                    local nx=$(( x + dx_off ))
                    local ny=$(( y + dy_off ))
                    local nz=$(( z + dz_off ))
                    local new_size=$half

                    # Count nanobots intersecting this sub-cube
                    local sub_count=0
                    for (( i=0; i<num_bots; i++ )); do
                        local bx=${bots_x[i]} by=${bots_y[i]} bz=${bots_z[i]} br=${bots_r[i]}
                        local nx_max=$(( nx + new_size - 1 ))
                        local ny_max=$(( ny + new_size - 1 ))
                        local nz_max=$(( nz + new_size - 1 ))
                        local dist_to_cube=0
                        local d_temp=0

                        if (( bx < nx )); then d_temp=$(( nx - bx )); elif (( bx > nx_max )); then d_temp=$(( bx - nx_max )); else d_temp=0; fi
                        dist_to_cube=$(( dist_to_cube + d_temp ))

                        if (( by < ny )); then d_temp=$(( ny - by )); elif (( by > ny_max )); then d_temp=$(( by - ny_max )); else d_temp=0; fi
                        dist_to_cube=$(( dist_to_cube + d_temp ))

                        if (( bz < nz )); then d_temp=$(( nz - bz )); elif (( bz > nz_max )); then d_temp=$(( bz - nz_max )); else d_temp=0; fi
                        dist_to_cube=$(( dist_to_cube + d_temp ))

                        if (( dist_to_cube <= br )); then
                            ((sub_count++))
                        fi
                    done

                    # Only add to heap if potentially better than best found
                    if (( best_count == -1 || sub_count >= best_count )); then
                         local sub_dist=$(min_distance_to_origin "$nx" "$ny" "$nz" "$new_size")
                         # Push sub-cube onto heap (append to file)
                         echo "$(( -sub_count )) $sub_dist $new_size $nx $ny $nz" >> "$heap_file"
                    fi
                done
            done
        done
    done

    echo "$best_distance"
}


# --- Main Execution ---
main() {
    local input_file="input.txt"
    if [[ ! -f "$input_file" ]]; then
        echo "Error: Input file '$input_file' not found." >&2
        exit 1
    fi

    parse_input "$input_file"

    local result_one
    result_one=$(part_one)
    echo "Part One: $result_one"

    # Part Two can be very slow in Bash due to heap simulation
    local result_two
    result_two=$(part_two)
    echo "Part Two: $result_two"

    # Explicitly clean up temp file if script exits normally
    # trap handles abnormal exits
    rm -f "$heap_file"
    trap - EXIT
}

# Run main
main
