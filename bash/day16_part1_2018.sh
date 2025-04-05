
#!/bin/bash

# --- Operation Functions ---
# Modify the global 'registers' array
# Args: A, B, C

addr() { (( registers[$3] = registers[$1] + registers[$2] )); }
addi() { (( registers[$3] = registers[$1] + $2 )); }
mulr() { (( registers[$3] = registers[$1] * registers[$2] )); }
muli() { (( registers[$3] = registers[$1] * $2 )); }
banr() { (( registers[$3] = registers[$1] & registers[$2] )); }
bani() { (( registers[$3] = registers[$1] & $2 )); }
borr() { (( registers[$3] = registers[$1] | registers[$2] )); }
bori() { (( registers[$3] = registers[$1] | $2 )); }
setr() { (( registers[$3] = registers[$1] )); }
seti() { (( registers[$3] = $1 )); }
gtir() { (( registers[$3] = ($1 > registers[$2]) ? 1 : 0 )); }
gtri() { (( registers[$3] = (registers[$1] > $2) ? 1 : 0 )); }
gtrr() { (( registers[$3] = (registers[$1] > registers[$2]) ? 1 : 0 )); }
eqir() { (( registers[$3] = ($1 == registers[$2]) ? 1 : 0 )); }
eqri() { (( registers[$3] = (registers[$1] == $2) ? 1 : 0 )); }
eqrr() { (( registers[$3] = (registers[$1] == registers[$2]) ? 1 : 0 )); }

# List of operation function names
operations=(addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr)

# --- Main Logic ---
main() {
    local total_count=0
    local state="idle"
    local line
    declare -a before instruction after registers

    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ "$line" == Before:* ]]; then
            local parsed_line="${line#Before: \[}"
            parsed_line="${parsed_line%]}"
            parsed_line="${parsed_line//, / }"
            read -r -a before <<< "$parsed_line"
            state="before_read"
        elif [[ "$state" == "before_read" && "$line" =~ ^[0-9]+[[:space:]][0-9]+[[:space:]][0-9]+[[:space:]][0-9]+$ ]]; then
            read -r -a instruction <<< "$line"
            state="instruction_read"
        elif [[ "$state" == "instruction_read" && "$line" == After:* ]]; then
            local parsed_line="${line#After:  \[}" # Note: often two spaces after After:
            parsed_line="${parsed_line%]}"
            parsed_line="${parsed_line//, / }"
            read -r -a after <<< "$parsed_line"

            local matches=0
            local op_name
            local A=${instruction[1]}
            local B=${instruction[2]}
            local C=${instruction[3]}

            for op_name in "${operations[@]}"; do
                registers=("${before[@]}")
                "$op_name" "$A" "$B" "$C"
                # Compare array contents as space-separated strings
                if [[ "${registers[*]}" == "${after[*]}" ]]; then
                    (( matches++ ))
                fi
            done

            if (( matches >= 3 )); then
                (( total_count++ ))
            fi
            state="idle"
        else
             # If line is blank or doesn't match expected sequence, potentially reset state
             if [[ -z "$line" ]]; then
                 : # Ignore blank lines
             elif [[ "$state" != "idle" ]]; then # Reset if we were expecting something specific
                state="idle"
             fi
        fi
    done < "input.txt"

    echo "$total_count"
}

# --- Entry Point ---
main
