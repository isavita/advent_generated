
#!/usr/bin/env bash
read -r line < input.txt
IFS=' ' read -r -a stones <<< "$line"
for _ in {1..25}; do
    new=()
    for s in "${stones[@]}"; do
        if [[ $s == 0 ]]; then
            new+=("1")
        elif (( ${#s} % 2 == 0 )); then
            mid=$(( ${#s} / 2 ))
            left=${s:0:mid}
            right=${s:mid}
            lead="${left%%[!0]*}"
            left_trim=${left#$lead}
            [[ -z $left_trim ]] && left_trim=0
            lead="${right%%[!0]*}"
            right_trim=${right#$lead}
            [[ -z $right_trim ]] && right_trim=0
            new+=("$left_trim" "$right_trim")
        else
            new+=("$((s * 2024))")
        fi
    done
    stones=("${new[@]}")
done
echo "${#stones[@]}"
