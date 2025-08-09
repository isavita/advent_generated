
#!/usr/bin/env bash

# read all test cases
declare -a targets numbers
total=0

while IFS= read -r line; do
    [[ -z $line ]] && continue
    # split target and numbers
    target=${line%%:*}
    rest=${line#*:}
    # trim spaces
    target=${target//[[:space:]]/}
    rest=${rest## }
    # read numbers into array
    IFS=' ' read -r -a nums <<< "$rest"
    # globals for recursion
    target_val=$target
    nums=("${tmp[@]}")
    nums=("${!tmp[@]}")
    nums=("${!tmp[@]}")
    nums=("${!tmp[@]}")
    # store for later processing
    numbers+=("$target")
    # store numbers as a single string (space‑separated) for later use
    numbers+=("${rest}")
done < input.txt

# recursion function
dfs() {
    local idx=$1 cur=$2
    if (( idx == ${#nums[@]} )); then
        (( cur == target_val )) && return 0 || return 1
    fi
    local nxt=${nums[idx]}

    # +
    local sum=$(( cur + nxt ))
    dfs $((idx+1)) $sum && return 0

    # *
    local prod=$(( cur * nxt ))
    dfs $((idx+1)) $prod && return 0

    # ||
    local concat="${cur}${nxt}"
    # bash arithmetic treats numbers as signed 64‑bit; overflow is ignored
    local cat=$((10#$concat))
    dfs $((idx+1)) $cat && return 0

    return 1
}

# process each test case
for ((i=0; i<${#numbers[@]}; i+=2)); do
    target=${numbers[i]}
    IFS=' ' read -r -a nums <<< "${numbers[i+1]}"
    # if only one number, just compare
    if (( ${#nums[@]} == 1 )); then
        (( ${nums[0]} == target )) && total=$(( total + target ))
        continue
    fi
    # set globals for recursion
    target_val=$target
    # copy numbers to global array
    nums=("${nums[@]}")
    # start recursion with first number
    dfs 1 "${nums[0]}" && total=$(( total + target ))
done

echo "$total"
