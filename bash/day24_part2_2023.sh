
#!/bin/bash

set -eo pipefail

# --- Configuration ---
INPUT_FILE="input.txt"
BC_SCALE=50 # Precision for bc calculations

# --- bc Helper ---
# Usage: result=$(bc_op "$num1 + $num2")
bc_op() {
    echo "scale=${BC_SCALE}; $1" | bc
}

# --- Vector Operations ---
# All vector functions take components as arguments and echo space-separated result components

# vec_add x1 y1 z1 x2 y2 z2
vec_add() {
    local x1=$1 y1=$2 z1=$3 x2=$4 y2=$5 z2=$6
    local rx=$(bc_op "$x1 + $x2")
    local ry=$(bc_op "$y1 + $y2")
    local rz=$(bc_op "$z1 + $z2")
    echo "$rx $ry $rz"
}

# vec_sub x1 y1 z1 x2 y2 z2
vec_sub() {
    local x1=$1 y1=$2 z1=$3 x2=$4 y2=$5 z2=$6
    local rx=$(bc_op "$x1 - $x2")
    local ry=$(bc_op "$y1 - $y2")
    local rz=$(bc_op "$z1 - $z2")
    echo "$rx $ry $rz"
}

# vec_mul x y z s
vec_mul() {
    local x=$1 y=$2 z=$3 s=$4
    local rx=$(bc_op "$x * $s")
    local ry=$(bc_op "$y * $s")
    local rz=$(bc_op "$z * $s")
    echo "$rx $ry $rz"
}

# vec_div x y z s
vec_div() {
    local x=$1 y=$2 z=$3 s=$4
    # Avoid division by zero, though Python code assumes it won't happen here
    if (( $(echo "$s == 0" | bc -l) )); then
        echo "Error: Division by zero" >&2
        exit 1
    fi
    local rx=$(bc_op "$x / $s")
    local ry=$(bc_op "$y / $s")
    local rz=$(bc_op "$z / $s")
    echo "$rx $ry $rz"
}

# vec_cross x1 y1 z1 x2 y2 z2
vec_cross() {
    local x1=$1 y1=$2 z1=$3 x2=$4 y2=$5 z2=$6
    # rx = y1*z2 - z1*y2
    # ry = z1*x2 - x1*z2
    # rz = x1*y2 - y1*x2
    local rx=$(bc_op "($y1 * $z2) - ($z1 * $y2)")
    local ry=$(bc_op "($z1 * $x2) - ($x1 * $z2)")
    local rz=$(bc_op "($x1 * $y2) - ($y1 * $x2)")
    echo "$rx $ry $rz"
}

# vec_dot x1 y1 z1 x2 y2 z2
vec_dot() {
    local x1=$1 y1=$2 z1=$3 x2=$4 y2=$5 z2=$6
    # result = x1*x2 + y1*y2 + z1*z2
    local r=$(bc_op "($x1 * $x2) + ($y1 * $y2) + ($z1 * $z2)")
    echo "$r"
}

# --- Hailstone Relative Operations ---

# hailstone_sub p1x p1y p1z v1x v1y v1z p2x p2y p2z v2x v2y v2z
# Echos rp_x rp_y rp_z rv_x rv_y rv_z
hailstone_sub() {
    local p1x=$1 p1y=$2 p1z=$3 v1x=$4 v1y=$5 v1z=$6
    local p2x=$7 p2y=$8 p2z=$9 v2x=${10} v2y=${11} v2z=${12}

    local rp=$(vec_sub "$p1x" "$p1y" "$p1z" "$p2x" "$p2y" "$p2z")
    local rv=$(vec_sub "$v1x" "$v1y" "$v1z" "$v2x" "$v2y" "$v2z")
    echo "$rp $rv"
}

# intersectionTime rpx rpy rpz rvx rvy rvz spx spy spz svx svy svz
# r = reference hailstone (relative), s = subject hailstone (relative)
intersectionTime() {
    local rpx=$1 rpy=$2 rpz=$3 rvx=$4 rvy=$5 rvz=$6
    local spx=$7 spy=$8 spz=$9 svx=${10} svy=${11} svz=${12}

    # Calculate plane normal vector: r.p x (r.p + r.v)
    local rp_add_rv=$(vec_add "$rpx" "$rpy" "$rpz" "$rvx" "$rvy" "$rvz")
    read -r rp_add_rv_x rp_add_rv_y rp_add_rv_z <<< "$rp_add_rv"
    local plane=$(vec_cross "$rpx" "$rpy" "$rpz" "$rp_add_rv_x" "$rp_add_rv_y" "$rp_add_rv_z")
    read -r plane_x plane_y plane_z <<< "$plane"

    # Calculate numerator: -s.p . plane
    local s_p_dot_plane=$(vec_dot "$spx" "$spy" "$spz" "$plane_x" "$plane_y" "$plane_z")
    local numerator=$(bc_op "-1 * ($s_p_dot_plane)")

    # Calculate denominator: s.v . plane
    local s_v_dot_plane=$(vec_dot "$svx" "$svy" "$svz" "$plane_x" "$plane_y" "$plane_z")
    local denominator=$s_v_dot_plane

    # Calculate time = numerator / denominator
    if (( $(echo "$denominator == 0" | bc -l) )); then
         echo "Error: Division by zero in intersectionTime" >&2
         exit 1
    fi
    local time=$(bc_op "$numerator / $denominator")
    echo "$time"
}

# --- Main Logic ---
main() {
    # Read first 3 hailstones from input.txt
    local -a p v
    local i=0
    while IFS= read -r line && [[ $i -lt 3 ]]; do
        # Extract numbers: px py pz @ vx vy vz
        local nums=($(echo "$line" | grep -oE -- '-?[0-9]+'))
        if [[ ${#nums[@]} -ne 6 ]]; then
            echo "Error: Invalid input line format: $line" >&2
            exit 1
        fi
        p[$i]="${nums[0]} ${nums[1]} ${nums[2]}"
        v[$i]="${nums[3]} ${nums[4]} ${nums[5]}"
        ((i++))
    done < "$INPUT_FILE"

    if [[ $i -ne 3 ]]; then
        echo "Error: Need at least 3 hailstones in $INPUT_FILE" >&2
        exit 1
    fi

    # Assign P/V components for easier access (p0x, p0y, ..., v0x, v0y, ...)
    read -r p0x p0y p0z <<< "${p[0]}"
    read -r v0x v0y v0z <<< "${v[0]}"
    read -r p1x p1y p1z <<< "${p[1]}"
    read -r v1x v1y v1z <<< "${v[1]}"
    read -r p2x p2y p2z <<< "${p[2]}"
    read -r v2x v2y v2z <<< "${v[2]}"

    # Calculate relative hailstones (s1-s0 and s2-s0)
    local ref1=$(hailstone_sub "$p1x" "$p1y" "$p1z" "$v1x" "$v1y" "$v1z" "$p0x" "$p0y" "$p0z" "$v0x" "$v0y" "$v0z")
    read -r ref1px ref1py ref1pz ref1vx ref1vy ref1vz <<< "$ref1"

    local ref2=$(hailstone_sub "$p2x" "$p2y" "$p2z" "$v2x" "$v2y" "$v2z" "$p0x" "$p0y" "$p0z" "$v0x" "$v0y" "$v0z")
    read -r ref2px ref2py ref2pz ref2vx ref2vy ref2vz <<< "$ref2"

    # Calculate intersection times
    local t1=$(intersectionTime "$ref2px" "$ref2py" "$ref2pz" "$ref2vx" "$ref2vy" "$ref2vz" "$ref1px" "$ref1py" "$ref1pz" "$ref1vx" "$ref1vy" "$ref1vz")
    local t2=$(intersectionTime "$ref1px" "$ref1py" "$ref1pz" "$ref1vx" "$ref1vy" "$ref1vz" "$ref2px" "$ref2py" "$ref2pz" "$ref2vx" "$ref2vy" "$ref2vz")

    # Calculate rock collision points
    # rock1 = s1.p + s1.v * t1
    local v1_mul_t1=$(vec_mul "$v1x" "$v1y" "$v1z" "$t1")
    local rock1=$(vec_add "$p1x" "$p1y" "$p1z" $v1_mul_t1)
    read -r r1x r1y r1z <<< "$rock1"

    # rock2 = s2.p + s2.v * t2
    local v2_mul_t2=$(vec_mul "$v2x" "$v2y" "$v2z" "$t2")
    local rock2=$(vec_add "$p2x" "$p2y" "$p2z" $v2_mul_t2)
    read -r r2x r2y r2z <<< "$rock2"

    # Calculate rock initial position (rp)
    # rp = rock1 - (rock2 - rock1) / (t2 - t1) * t1
    local r2_sub_r1=$(vec_sub "$r2x" "$r2y" "$r2z" "$r1x" "$r1y" "$r1z")
    read -r drx dry drz <<< "$r2_sub_r1"
    local t2_sub_t1=$(bc_op "$t2 - $t1")

    # Check for t1 == t2 (parallel relative paths, shouldn't happen with valid input)
     if (( $(echo "$t2_sub_t1 == 0" | bc -l) )); then
         echo "Error: t1 equals t2, cannot determine rock velocity." >&2
         exit 1
    fi

    local rock_vel=$(vec_div "$drx" "$dry" "$drz" "$t2_sub_t1")
    read -r rvx rvy rvz <<< "$rock_vel"

    local rock_vel_mul_t1=$(vec_mul "$rvx" "$rvy" "$rvz" "$t1")
    read -r rv_mul_t1_x rv_mul_t1_y rv_mul_t1_z <<< "$rock_vel_mul_t1"

    local rp=$(vec_sub "$r1x" "$r1y" "$r1z" "$rv_mul_t1_x" "$rv_mul_t1_y" "$rv_mul_t1_z")
    read -r rpx rpy rpz <<< "$rp"

    # Calculate sum of rock position components and print integer part
    local final_sum=$(bc_op "$rpx + $rpy + $rpz")
    
    # Use bc to truncate towards zero, mimicking Python's int()
    echo "scale=0; $final_sum / 1" | bc
}

# --- Entry Point ---
main
