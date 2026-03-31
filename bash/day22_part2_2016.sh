
#!/bin/bash

max_x=0
hole_x=0
hole_y=0
wall_min_x=999

while read -r x y used; do
    [[ $x -gt $max_x ]] && max_x=$x
    if [[ $used -eq 0 ]]; then
        hole_x=$x
        hole_y=$y
    fi
    if [[ $used -gt 400 ]]; then
        [[ $x -lt $wall_min_x ]] && wall_min_x=$x
    fi
done < <(grep "/dev/grid/node" input.txt | sed 's/[^0-9 ]/ /g' | awk '{print $1, $2, $4}')

# Calculate distance for the hole to go around the wall to the node left of the target data
# 1. Move hole left to clear the wall
# 2. Move hole up to y=0
# 3. Move hole right to max_x - 1
dist_to_target=$(( (hole_x - (wall_min_x - 1)) + hole_y + (max_x - 1 - (wall_min_x - 1)) ))

# 1 move to swap data into the hole, then 5 moves for each step left to (0,0)
total_moves=$(( dist_to_target + 1 + (max_x - 1) * 5 ))

echo $total_moves
