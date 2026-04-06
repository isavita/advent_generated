
#!/bin/bash

jet=$(<input.txt)
jet=${jet//[$'\r\n']/}
jet_len=${#jet}
jet_ptr=0

declare -a tower
tower[0]=127
h_y=0

s0=(15); s1=(2 7 2); s2=(7 4 4); s3=(1 1 1 1); s4=(3 3)

for ((n=0; n<2022; n++)); do
    case $((n % 5)) in
        0) sr=("${s0[@]}"); sw=4; sh=1 ;;
        1) sr=("${s1[@]}"); sw=3; sh=3 ;;
        2) sr=("${s2[@]}"); sw=3; sh=3 ;;
        3) sr=("${s3[@]}"); sw=1; sh=4 ;;
        4) sr=("${s4[@]}"); sw=2; sh=2 ;;
    esac

    cx=2
    cy=$((h_y + 4))

    while :; do
        char=${jet:jet_ptr:1}
        ((++jet_ptr == jet_len)) && jet_ptr=0

        if [[ "$char" == ">" ]]; then
            if (( cx + sw < 7 )); then
                coll=0
                ncx=$((cx + 1))
                for ((i=0; i<sh; i++)); do
                    (( (tower[cy+i] & (sr[i] << ncx)) )) && { coll=1; break; }
                done
                (( coll == 0 )) && cx=$ncx
            fi
        else
            if (( cx > 0 )); then
                coll=0
                ncx=$((cx - 1))
                for ((i=0; i<sh; i++)); do
                    (( (tower[cy+i] & (sr[i] << ncx)) )) && { coll=1; break; }
                done
                (( coll == 0 )) && cx=$ncx
            fi
        fi

        coll=0
        ncy=$((cy - 1))
        for ((i=0; i<sh; i++)); do
            (( (tower[ncy+i] & (sr[i] << cx)) )) && { coll=1; break; }
        done

        if (( coll == 1 )); then
            for ((i=0; i<sh; i++)); do
                idx=$((cy + i))
                tower[idx]=$(( tower[idx] | (sr[i] << cx) ))
                (( idx > h_y )) && h_y=$idx
            done
            break
        else
            cy=$ncy
        fi
    done
done

echo $h_y
