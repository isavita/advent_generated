
#!/bin/bash

packages=($(sort -rn input.txt))
total=0
for x in "${packages[@]}"; do ((total+=x)); done
target=$((total / 3))
n=${#packages[@]}
min_qe=-1

find_combinations() {
    local depth=$1 start=$2 current_sum=$3 current_qe=$4
    if (( depth == 0 )); then
        if (( current_sum == target )); then
            [[ $min_qe == -1 || $current_qe -lt $min_qe ]] && min_qe=$current_qe
        fi
        return
    fi

    for ((i=start; i <= n - depth; i++)); do
        local v=${packages[i]}
        local next_sum=$((current_sum + v))
        
        (( next_sum > target )) && continue
        (( next_sum + v * (depth - 1) < target )) && break
        [[ $min_qe != -1 ]] && (( current_qe * v >= min_qe )) && continue
        
        find_combinations "$((depth - 1))" "$((i + 1))" "$next_sum" "$((current_qe * v))"
    done
}

for ((len=1; len<n; len++)); do
    find_combinations "$len" 0 0 1
    if [[ $min_qe != -1 ]]; then
        echo "$min_qe"
        exit
    fi
done
