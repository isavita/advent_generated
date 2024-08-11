#!/bin/bash

# Function to increment the password
increment_password() {
    local password="$1"
    local i
    for (( i=${#password}-1; i>=0; i-- )); do
        local char="${password:i:1}"
        if [[ "$char" == "z" ]]; then
            password="${password:0:i}a${password:i+1}"
        else
            local next_char=$(printf "%b" "$(printf '%d' "'$char")" | awk '{printf("%c", $1+1)}')
            password="${password:0:i}$next_char${password:i+1}"
            break
        fi
    done
    echo "$password"
}

# Function to check if the password contains a straight of at least three letters
contains_straight() {
    local password="$1"
    local i
    for (( i=0; i<${#password}-2; i++ )); do
        local char1="${password:i:1}"
        local char2="${password:i+1:1}"
        local char3="${password:i+2:1}"
        if [[ "$char1" < "$char2" && "$char2" < "$char3" ]]; then
            return 0
        fi
    done
    return 1
}

# Function to check if the password contains at least two different, non-overlapping pairs of letters
contains_pairs() {
    local password="$1"
    local pairs=0
    local i
    local last_pair=""
    for (( i=0; i<${#password}-1; i++ )); do
        local char1="${password:i:1}"
        local char2="${password:i+1:1}"
        if [[ "$char1" == "$char2" ]]; then
            if [[ "$last_pair" != "$char1" ]]; then
                ((pairs++))
                last_pair="$char1"
            fi
        fi
    done
    [[ $pairs -ge 2 ]]
}

# Function to check if the password is valid
is_valid_password() {
    local password="$1"
    if [[ "$password" =~ [iol] ]]; then
        return 1
    fi
    if ! contains_straight "$password"; then
        return 1
    fi
    if ! contains_pairs "$password"; then
        return 1
    fi
    return 0
}

# Read the password from input.txt
password=$(<input.txt)

# Increment the password until a valid one is found
while ! is_valid_password "$password"; do
    password=$(increment_password "$password")
done

# Print the valid password
echo "$password"