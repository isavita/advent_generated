
#!/bin/bash

# Function to calculate the absolute value of a number
abs() {
    local val=$1
    if (( val < 0 )); then
        echo $(( -val ))
    else
        echo "$val"
    fi
}

main() {
    local x=0
    local y=0
    # Store vertices as parallel arrays
    local -a vertices_x=(0)
    local -a vertices_y=(0)
    local length
    local dir_input

    # Read input file line by line
    while IFS=' ' read -r dir_input length _; do
        # Update coordinates based on direction and length
        case "$dir_input" in
            U) y=$((y - length)) ;;
            L) x=$((x - length)) ;;
            D) y=$((y + length)) ;;
            R) x=$((x + length)) ;;
        esac
        # Add new vertex
        vertices_x+=("$x")
        vertices_y+=("$y")
    done < "input.txt"

    local n=${#vertices_x[@]}
    local area_sum=0
    local perim=0
    local i next
    local x_i y_i x_next y_next
    local dx dy abs_dx abs_dy

    # Calculate Shoelace formula sum and perimeter in a single loop
    for (( i=0; i<n; i++ )); do
        next=$(((i + 1) % n)) # Wrap around for the last vertex

        x_i=${vertices_x[i]}
        y_i=${vertices_y[i]}
        x_next=${vertices_x[next]}
        y_next=${vertices_y[next]}

        # Shoelace formula component
        area_sum=$((area_sum + x_i * y_next - y_i * x_next))

        # Perimeter component (Manhattan distance)
        dx=$((x_i - x_next))
        dy=$((y_i - y_next))
        abs_dx=$(abs "$dx")
        abs_dy=$(abs "$dy")
        perim=$((perim + abs_dx + abs_dy))
    done

    # Final Shoelace area calculation (absolute value and divide by 2)
    local shoelace_area
    shoelace_area=$(abs "$area_sum")
    shoelace_area=$((shoelace_area / 2))

    # Calculate total area using Pick's Theorem rearranged: Area = Shoelace_Area + Perimeter / 2 + 1
    local total_area=$((shoelace_area + perim / 2 + 1))

    echo "$total_area"
}

# Execute the main function
main
