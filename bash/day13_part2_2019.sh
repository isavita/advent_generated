
#!/bin/bash

IFS=',' read -r -a mem < input.txt
mem[0]=2

ip=0 rb=0 score=0 ball_x=0 paddle_x=0 out_step=0

while :; do
    ins=${mem[ip]}
    op=$((ins % 100))
    m1=$((ins / 100 % 10))
    m2=$((ins / 1000 % 10))
    m3=$((ins / 10000 % 10))

    [[ $op -eq 99 ]] && break

    v1=${mem[ip+1]:-0}
    v2=${mem[ip+2]:-0}
    v3=${mem[ip+3]:-0}

    case $m1 in 0) p1=${mem[v1]:-0} ;; 1) p1=$v1 ;; 2) p1=${mem[rb+v1]:-0} ;; esac
    case $m2 in 0) p2=${mem[v2]:-0} ;; 1) p2=$v2 ;; 2) p2=${mem[rb+v2]:-0} ;; esac

    case $op in
        1)  case $m3 in 0) a=$v3 ;; 2) a=$((rb+v3)) ;; esac
            mem[a]=$((p1 + p2)); ip=$((ip+4)) ;;
        2)  case $m3 in 0) a=$v3 ;; 2) a=$((rb+v3)) ;; esac
            mem[a]=$((p1 * p2)); ip=$((ip+4)) ;;
        3)  case $m1 in 0) a=$v1 ;; 2) a=$((rb+v1)) ;; esac
            joy=0; (( ball_x > paddle_x )) && joy=1; (( ball_x < paddle_x )) && joy=-1
            mem[a]=$joy; ip=$((ip+2)) ;;
        4)  if (( out_step == 0 )); then ox=$p1; out_step=1
            elif (( out_step == 1 )); then oy=$p1; out_step=2
            else
                if (( ox == -1 && oy == 0 )); then score=$p1
                elif (( p1 == 3 )); then paddle_x=$ox
                elif (( p1 == 4 )); then ball_x=$ox
                fi
                out_step=0
            fi; ip=$((ip+2)) ;;
        5)  (( p1 != 0 )) && ip=$p2 || ip=$((ip+3)) ;;
        6)  (( p1 == 0 )) && ip=$p2 || ip=$((ip+3)) ;;
        7)  case $m3 in 0) a=$v3 ;; 2) a=$((rb+v3)) ;; esac
            (( p1 < p2 )) && mem[a]=1 || mem[a]=0; ip=$((ip+4)) ;;
        8)  case $m3 in 0) a=$v3 ;; 2) a=$((rb+v3)) ;; esac
            (( p1 == p2 )) && mem[a]=1 || mem[a]=0; ip=$((ip+4)) ;;
        9)  rb=$((rb + p1)); ip=$((ip+2)) ;;
    esac
done
echo $score
