
#!/bin/bash

main() {
    local cycle=0
    local x_reg=1
    local crt_output=""

    draw_pixel() {
        local crt_pos=$(( cycle % 40 ))
        local pixel="."
        
        if (( crt_pos >= x_reg - 1 && crt_pos <= x_reg + 1 )); then
            pixel="#"
        fi
        
        crt_output+="$pixel"

        if (( crt_pos == 39 )); then
            echo "$crt_output"
            crt_output=""
        fi
        
        cycle=$((cycle + 1))
    }

    while read -r instruction value || [[ -n "$instruction" ]]; do
        if [[ "$instruction" == "noop" ]]; then
            draw_pixel
        elif [[ "$instruction" == "addx" ]]; then
            draw_pixel 
            draw_pixel 
            x_reg=$((x_reg + value))
        fi
        instruction="" 
    done < "input.txt"
}

main
