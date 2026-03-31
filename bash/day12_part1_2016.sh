
#!/bin/bash

a=0; b=0; c=0; d=0; pc=0; ops=(); a1=(); a2=()

while read -r o r1 r2 || [[ -n $o ]]; do
    [[ -z $o ]] && continue
    ops+=("$o"); a1+=("$r1"); a2+=("$r2")
done < input.txt

n=${#ops[@]}
while ((pc < n)); do
    o=${ops[pc]}; r1=${a1[pc]}; r2=${a2[pc]}
    case $o in
        cpy) 
            case $r1 in [a-d]) v=${!r1} ;; *) v=$r1 ;; esac
            printf -v "$r2" %s "$v"; ((pc++)) ;;
        inc) 
            (($r1++, pc++)) ;;
        dec) 
            (($r1--, pc++)) ;;
        jnz) 
            case $r1 in [a-d]) v=${!r1} ;; *) v=$r1 ;; esac
            ((pc += (v != 0 ? r2 : 1))) ;;
    esac
done

echo $a
