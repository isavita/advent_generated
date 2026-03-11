
#!/usr/bin/awk -f

# Day 10: Monitoring Station

BEGIN {
    # Set input file name
    if (ARGC < 2) {
        ARGV[1] = "input.txt"
        ARGC = 2
    }
    y_coord = 0
    asteroid_count = 0
}

{
    # Parse each character of the input row to map asteroid positions
    row_len = length($0)
    for (x_coord = 0; x_coord < row_len; x_coord++) {
        char = substr($0, x_coord + 1, 1)
        if (char == "#") {
            posX[asteroid_count] = x_coord
            posY[asteroid_count] = y_coord
            asteroid_count++
        }
    }
    y_coord++
}

END {
    if (asteroid_count == 0) exit

    # --- Part One: Best Monitoring Station Location ---
    # Determine which asteroid can detect the largest number of other asteroids.
    max_visible = -1
    best_asteroid_idx = -1

    for (i = 0; i < asteroid_count; i++) {
        split("", seen_angles)
        current_visible = 0
        for (j = 0; j < asteroid_count; j++) {
            if (i == j) continue
            dx = posX[j] - posX[i]
            dy = posY[j] - posY[i]
            
            # Calculate angle to use as a unique slope key
            # atan2 returns value in range (-PI, PI]
            angle_key = sprintf("%.12f", atan2(dy, dx))
            if (!(angle_key in seen_angles)) {
                seen_angles[angle_key] = 1
                current_visible++
            }
        }
        if (current_visible > max_visible) {
            max_visible = current_visible
            best_asteroid_idx = i
        }
    }

    print max_visible

    # --- Part Two: Vaporization Rotation ---
    # Starting from the top and rotating clockwise, find the 200th vaporized asteroid.
    stationX = posX[best_asteroid_idx]
    stationY = posY[best_asteroid_idx]
    PI = atan2(0, -1)
    
    target_list_size = 0
    for (i = 0; i < asteroid_count; i++) {
        if (i == best_asteroid_idx) continue
        dx = posX[i] - stationX
        dy = posY[i] - stationY
        
        # atan2(dx, -dy) maps Up (0, -1) to 0, Right (1, 0) to PI/2, and so on.
        angle = atan2(dx, -dy)
        if (angle < 0) angle += 2 * PI
        
        # Calculate squared distance for tie-breaking same angles
        dist_sq = dx*dx + dy*dy
        
        # Format strings for lexicographical sorting: Angle ASC, then Distance ASC
        targets[target_list_size++] = sprintf("%15.10f %15.10f %d %d", angle, dist_sq, posX[i], posY[i])
    }

    # Use a standard quicksort to order targets by angle and distance
    srand()
    perform_quicksort(targets, 0, target_list_size - 1)

    vaporized_total = 0
    split("", is_vaporized)
    
    # Loop until the 200th asteroid is vaporized
    while (vaporized_total < target_list_size) {
        last_shot_angle = -1.0
        for (i = 0; i < target_list_size; i++) {
            if (is_vaporized[i]) continue
            
            split(targets[i], target_data, " ")
            curr_angle = target_data[1] + 0
            
            # The laser hits only one asteroid per unique angle per rotation
            if (curr_angle > last_shot_angle + 1e-15) {
                is_vaporized[i] = 1
                vaporized_total++
                last_shot_angle = curr_angle
                
                # Check for the 200th target
                if (vaporized_total == 200) {
                    finalX = target_data[3]
                    finalY = target_data[4]
                    print (finalX * 100 + finalY)
                    exit
                }
            }
        }
    }
}

# Hoare quicksort implementation for numeric string arrays
function perform_quicksort(arr, left, right,   i, last) {
    if (left >= right) return
    swap_elements(arr, left, left + int((right - left + 1) * rand()))
    last = left
    for (i = left + 1; i <= right; i++) {
        if (arr[i] < arr[left]) {
            swap_elements(arr, ++last, i)
        }
    }
    swap_elements(arr, left, last)
    perform_quicksort(arr, left, last - 1)
    perform_quicksort(arr, last + 1, right)
}

function swap_elements(arr, i, j,   temp) {
    temp = arr[i]
    arr[i] = arr[j]
    arr[j] = temp
}

