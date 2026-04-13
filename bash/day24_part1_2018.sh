
#!/bin/bash

# Army 1: Immune System, Army 2: Infection
declare -a g_army g_u g_hp g_d g_t g_i g_imm g_weak
id=0
curr=0

# Parse input.txt
while IFS= read -r line || [[ -n "$line" ]]; do
    [[ "$line" == "Immune System:" ]] && { curr=1; continue; }
    [[ "$line" == "Infection:" ]] && { curr=2; continue; }
    [[ -z "$line" ]] && continue
    ((id++))
    g_army[id]=$curr
    [[ "$line" =~ ^([0-9]+)\ units\ each\ with\ ([0-9]+)\ hit\ points ]] && { g_u[id]=${BASH_REMATCH[1]}; g_hp[id]=${BASH_REMATCH[2]}; }
    [[ "$line" =~ attack\ that\ does\ ([0-9]+)\ ([a-z]+)\ damage\ at\ initiative\ ([0-9]+) ]] && { g_d[id]=${BASH_REMATCH[1]}; g_t[id]=${BASH_REMATCH[2]}; g_i[id]=${BASH_REMATCH[3]}; }
    if [[ "$line" =~ \((.*)\) ]]; then
        IFS=';' read -ra parts <<< "${BASH_REMATCH[1]}"
        for p in "${parts[@]}"; do
            [[ "$p" =~ immune\ to\ (.*) ]] && g_imm[id]=" ${BASH_REMATCH[1]//,/ } "
            [[ "$p" =~ weak\ to\ (.*) ]] && g_weak[id]=" ${BASH_REMATCH[1]//,/ } "
        done
    fi
done < input.txt

# Simulation Loop
while :; do
    a1=0; a2=0; total=0
    for ((i=1; i<=id; i++)); do
        if (( g_u[i] > 0 )); then
            (( g_army[i] == 1 )) && a1=1 || a2=1
            (( total += g_u[i] ))
        fi
    done
    (( a1 == 0 || a2 == 0 )) && break

    # Target Selection
    order=$(for ((k=1; k<=id; k++)); do
        (( g_u[k] > 0 )) && echo "$(( g_u[k] * g_d[k] )) ${g_i[k]} $k"
    done | sort -k1,1nr -k2,2nr | cut -d' ' -f3)

    unset target_of is_targeted; declare -A target_of is_targeted
    for i in $order; do
        max_d=0; best_t=0; best_ep=0; best_i=0
        ep_i=$(( g_u[i] * g_d[i] ))
        for ((j=1; j<=id; j++)); do
            if (( g_u[j] <= 0 || g_army[j] == g_army[i] )) || [[ ${is_targeted[$j]} ]]; then continue; fi
            dmg=$ep_i
            if [[ "${g_imm[j]}" == *" ${g_t[i]} "* ]]; then dmg=0;
            elif [[ "${g_weak[j]}" == *" ${g_t[i]} "* ]]; then ((dmg *= 2)); fi
            (( dmg == 0 )) && continue
            ep_j=$(( g_u[j] * g_d[j] ))
            if (( dmg > max_d || (dmg == max_d && (ep_j > best_ep || (ep_j == best_ep && g_i[j] > best_i))) )); then
                max_d=$dmg; best_t=$j; best_ep=$ep_j; best_i=${g_i[j]}
            fi
        done
        if (( best_t > 0 )); then
            target_of[$i]=$best_t; is_targeted[$best_t]=1
        fi
    done

    # Attack Phase
    attack_order=$(for ((k=1; k<=id; k++)); do
        (( g_u[k] > 0 )) && echo "${g_i[k]} $k"
    done | sort -k1,1nr | cut -d' ' -f2)

    killed_any=0
    for i in $attack_order; do
        t=${target_of[$i]:-0}
        if (( t > 0 && g_u[i] > 0 )); then
            dmg=$(( g_u[i] * g_d[i] ))
            if [[ "${g_imm[t]}" == *" ${g_t[i]} "* ]]; then dmg=0;
            elif [[ "${g_weak[t]}" == *" ${g_t[i]} "* ]]; then ((dmg *= 2)); fi
            nkilled=$(( dmg / g_hp[t] ))
            (( nkilled > g_u[t] )) && nkilled=${g_u[t]}
            (( g_u[t] -= nkilled ))
            (( killed_any += nkilled ))
        fi
    done
    (( killed_any == 0 )) && break
done

# Result
total=0
for ((i=1; i<=id; i++)); do (( g_u[i] > 0 )) && (( total += g_u[i] )); done
echo $total
