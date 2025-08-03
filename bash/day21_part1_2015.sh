#!/usr/bin/env bash
set -euo pipefail

hp=$(awk -F': ' 'NR==1{print $2}' input.txt)
bd=$(awk -F': ' 'NR==2{print $2}' input.txt)
ba=$(awk -F': ' 'NR==3{print $2}' input.txt)

weapons_c=(8 10 25 40 74)
weapons_d=(4 5 6 7 8)

armors_c=(0 13 31 53 75 102)
armors_a=(0 1 2 3 4 5)

rings_c=(0 25 50 100 20 40 80)
rings_d=(0 1 2 3 0 0 0)
rings_a=(0 0 0 0 1 2 3)

min=99999999

for ((w=0; w<5; w++)); do
  wd=${weapons_d[w]}
  wc=${weapons_c[w]}
  for ((a=0; a<6; a++)); do
    aa=${armors_a[a]}
    ac=${armors_c[a]}
    for ((ri=0; ri<7; ri++)); do
      ridi=${rings_d[ri]}
      riai=${rings_a[ri]}
      rci=${rings_c[ri]}
      for ((rj=ri+1; rj<7; rj++)); do
        ridj=${rings_d[rj]}
        raij=${rings_a[rj]}
        rcj=${rings_c[rj]}

        pd=$((wd + ridi + ridj))
        pa=$((aa + riai + raij))
        cost=$((wc + ac + rci + rcj))

        pdmg=$((pd - ba)); ((pdmg<1)) && pdmg=1
        bdmg=$((bd - pa)); ((bdmg<1)) && bdmg=1

        pturns=$(( (hp + pdmg - 1) / pdmg ))
        bturns=$(( (100 + bdmg - 1) / bdmg ))

        if (( pturns <= bturns && cost < min )); then
          min=$cost
        fi
      done
    done
  done
done

echo "$min"