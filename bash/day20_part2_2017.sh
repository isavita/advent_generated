#!/bin/bash

# Read input from file
particles=()
while IFS= read -r line; do
  particles+=("$line")
done < input.txt

# Part 1: Find particle that will stay closest to <0,0,0> in the long term
closest_particle=0
min_distance=99999999
for ((i=0; i<${#particles[@]}; i++)); do
  p=(${particles[$i]//,/ })
  a=(${p[2]//,/ })
  distance=${a[0]}*${a[0]}+${a[1]}*${a[1]}+${a[2]}*${a[2]}
  if ((distance < min_distance)); then
    min_distance=$distance
    closest_particle=$i
  fi
done
echo "Part 1: Particle $closest_particle will stay closest to <0,0,0>"

# Part 2: Remove particles that collide
declare -A positions
particles_left=${#particles[@]}
for ((tick=0; tick<1000; tick++)); do
  for ((i=0; i<${#particles[@]}; i++)); do
    p=(${particles[$i]//,/ })
    v=(${p[1]//,/ })
    a=(${p[2]//,/ })
    v[0]=$((v[0] + a[0]))
    v[1]=$((v[1] + a[1]))
    v[2]=$((v[2] + a[2]))
    p[0]=$((p[0] + v[0]))
    p[1]=$((p[1] + v[1]))
    p[2]=$((p[2] + v[2]))
    key="${p[0]},${p[1]},${p[2]}"
    if [[ ${positions[$key]} ]]; then
      positions[$key]=$((positions[$key] + 1))
    else
      positions[$key]=1
    fi
  done
  for key in "${!positions[@]}"; do
    if ((positions[$key] > 1)); then
      for ((i=0; i<${#particles[@]}; i++)); do
        p=(${particles[$i]//,/ })
        v=(${p[1]//,/ })
        a=(${p[2]//,/ })
        if [[ "${p[0]},${p[1]},${p[2]}" == "$key" ]]; then
          unset "particles[$i]"
          ((particles_left--))
        fi
      done
    fi
  done
done
echo "Part 2: $particles_left particles are left after all collisions are resolved"