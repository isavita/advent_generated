
#!/bin/bash

# Simple, concise code, no comments or explanation.

main() {
    local num_twos=0
    local num_threes=0
    local line
    local has_two has_three

    if [[ ! -f "input.txt" ]]; then
        echo "Error: input.txt not found." >&2
        exit 1
    fi

    while IFS= read -r line || [[ -n "$line" ]]; do
        [[ -z "$line" ]] && continue

        read -r has_two has_three < <(echo "$line" | fold -w1 | sort | uniq -c | awk '
            BEGIN { t2=0; t3=0 }
            $1 == 2 { t2=1 }
            $1 == 3 { t3=1 }
            END { print t2, t3 }
        ')

        if [[ "$has_two" -eq 1 ]]; then
            ((num_twos++))
        fi
        if [[ "$has_three" -eq 1 ]]; then
            ((num_threes++))
        fi
    done < "input.txt"

    echo $((num_twos * num_threes))
}

main
