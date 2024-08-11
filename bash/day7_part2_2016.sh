#!/bin/bash

check_tls() {
    local ip="$1"
    local outside_brackets=0
    local has_abba=0
    local has_abba_in_brackets=0

    for segment in $(echo "$ip" | tr -s '[]' ' '); do
        if [[ "$segment" =~ ([a-z])([a-z])\2\1 ]]; then
            if (( outside_brackets )); then
                has_abba=1
            else
                has_abba_in_brackets=1
            fi
        fi
        outside_brackets=$((1 - outside_brackets))
    done

    (( has_abba && ! has_abba_in_brackets ))
}

check_ssl() {
    local ip="$1"
    local aba=()
    local bab=()
    local outside_brackets=0

    for segment in $(echo "$ip" | tr -s '[]' ' '); do
        if (( outside_brackets )); then
            for (( i=0; i<${#segment}-2; i++ )); do
                if [[ "${segment:i:1}" == "${segment:i+2:1}" && "${segment:i:1}" != "${segment:i+1:1}" ]]; then
                    aba+=("${segment:i:3}")
                fi
            done
        else
            for (( i=0; i<${#segment}-2; i++ )); do
                if [[ "${segment:i:1}" == "${segment:i+2:1}" && "${segment:i:1}" != "${segment:i+1:1}" ]]; then
                    bab+=("${segment:i:3}")
                fi
            done
        fi
        outside_brackets=$((1 - outside_brackets))
    done

    for a in "${aba[@]}"; do
        local b="${a:1:1}${a:0:1}${a:1:1}"
        [[ " ${bab[@]} " =~ " $b " ]] && return 0
    done
    return 1
}

tls_count=0
ssl_count=0

while read -r ip; do
    check_tls "$ip" && (( tls_count++ ))
    check_ssl "$ip" && (( ssl_count++ ))
done < input.txt

echo "TLS count: $tls_count"
echo "SSL count: $ssl_count"