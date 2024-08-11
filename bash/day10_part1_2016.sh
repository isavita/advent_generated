#!/bin/bash

declare -A bots actions outputs

while read -r line; do
    if [[ $line =~ value\ ([0-9]+)\ goes\ to\ bot\ ([0-9]+) ]]; then
        bots[${BASH_REMATCH[2]}]+="${BASH_REMATCH[1]} "
    elif [[ $line =~ bot\ ([0-9]+)\ gives\ low\ to\ (bot|output)\ ([0-9]+)\ and\ high\ to\ (bot|output)\ ([0-9]+) ]]; then
        actions[${BASH_REMATCH[1]}]="${BASH_REMATCH[2]} ${BASH_REMATCH[3]} ${BASH_REMATCH[4]} ${BASH_REMATCH[5]}"
    fi
done < input.txt

while true; do
    progress=false
    for bot in "${!bots[@]}"; do
        if [[ $(echo "${bots[$bot]}" | wc -w) -eq 2 ]]; then
            read -r low high <<< $(echo "${bots[$bot]}" | tr ' ' '\n' | sort -n | tr '\n' ' ')
            bots[$bot]=""
            IFS=' ' read -r target_type_low target_low target_type_high target_high <<< "${actions[$bot]}"
            if [[ $target_type_low == "bot" ]]; then
                bots[$target_low]+="$low "
            else
                outputs[$target_low]+="$low "
            fi
            if [[ $target_type_high == "bot" ]]; then
                bots[$target_high]+="$high "
            else
                outputs[$target_high]+="$high "
            fi
            [[ $low -eq 17 && $high -eq 61 ]] && echo "$bot"
            progress=true
        fi
    done
    [[ $progress == false ]] && break
done