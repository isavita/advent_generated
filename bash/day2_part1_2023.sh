#!/bin/bash

input_file="input.txt"
total_sum=0

while IFS= read -r line; do
    if [[ $line =~ Game\ ([0-9]+):\ (.+) ]]; then
        game_id=${BASH_REMATCH[1]}
        rounds=()
        IFS=';' read -ra rounds <<< "${BASH_REMATCH[2]}"
        is_valid=true

        for round in "${rounds[@]}"; do
            red=0
            green=0
            blue=0

            while [[ $round =~ ([0-9]+)\ (red|green|blue) ]]; do
                count=${BASH_REMATCH[1]}
                color=${BASH_REMATCH[2]}
                round=${round#*" "${color}}

                case "$color" in
                red) red=$((red + count)) ;;
                green) green=$((green + count)) ;;
                blue) blue=$((blue + count)) ;;
                esac

                if [ "$red" -gt 12 ] || [ "$green" -gt 13 ] || [ "$blue" -gt 14 ]; then
                    is_valid=false
                    break
                fi
            done

            if [ "$is_valid" = false ]; then
                break
            fi
        done

        if [ "$is_valid" = true ]; then
            total_sum=$((total_sum + game_id))
        fi
    fi
done < "$input_file"

echo "$total_sum"