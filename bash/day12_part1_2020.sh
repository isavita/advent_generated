
#!/bin/bash

main() {
    local x=0
    local y=0
    # 0=E, 1=S, 2=W, 3=N (Matches Python's directions list order)
    local current_dir_index=0 # Start East

    while IFS= read -r line || [[ -n "$line" ]]; do
        local action="${line:0:1}"
        local value="${line:1}"

        case "$action" in
            N) y=$(( y + value )) ;;
            S) y=$(( y - value )) ;;
            E) x=$(( x + value )) ;;
            W) x=$(( x - value )) ;;
            L)
                local turns=$(( value / 90 ))
                current_dir_index=$(( (current_dir_index - turns % 4 + 4) % 4 ))
                ;;
            R)
                local turns=$(( value / 90 ))
                current_dir_index=$(( (current_dir_index + turns) % 4 ))
                ;;
            F)
                case "$current_dir_index" in
                    0) x=$(( x + value )) ;; # E
                    1) y=$(( y - value )) ;; # S
                    2) x=$(( x - value )) ;; # W
                    3) y=$(( y + value )) ;; # N
                esac
                ;;
        esac
    done < "input.txt"

    local abs_x=$(( x < 0 ? -x : x ))
    local abs_y=$(( y < 0 ? -y : y ))
    echo $(( abs_x + abs_y ))
}

main
