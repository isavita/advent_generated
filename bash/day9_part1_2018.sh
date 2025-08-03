#!/usr/bin/env bash
set -euo pipefail

read players last < <(awk '{print $1, $7}' input.txt)

# Implement a circular doubly linked list using arrays
# prev[i], next[i] are indices; value is just i
max_nodes=$((last + 1))
declare -a prev next
declare -a scores
for ((i=0;i<players;i++)); do scores[i]=0; done

prev[0]=0
next[0]=0
current=0

for ((marble=1; marble<=last; marble++)); do
  if (( marble % 23 == 0 )); then
    player=$((marble % players))
    for ((i=0;i<7;i++)); do current=${prev[current]}; done
    scores[player]=$(( scores[player] + marble + current ))
    p=${prev[current]}
    n=${next[current]}
    next[p]=$n
    prev[n]=$p
    current=$n
  else
    c1=${next[current]}
    c2=${next[c1]}
    prev[marble]=$c1
    next[marble]=$c2
    next[c1]=$marble
    prev[c2]=$marble
    current=$marble
  fi
done

max=0
for ((i=0;i<players;i++)); do
  (( scores[i] > max )) && max=${scores[i]}
done
echo "$max"