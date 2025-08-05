#!/usr/bin/env bash
set -euo pipefail

key=$(tr -d '\r\n' < input.txt)

knot_hash() {
  local s="$1" lengths=() lst=() pos=0 skip=0 i j len start end t
  for ((i=0;i<${#s};i++)); do lengths+=($(printf '%d' "'${s:i:1}")); done
  lengths+=(17 31 73 47 23)
  for ((i=0;i<256;i++)); do lst[$i]=$i; done
  for ((i=0;i<64;i++)); do
    for len in "${lengths[@]}"; do
      for ((j=0;j<len/2;j++)); do
        start=$(( (pos + j) & 255 ))
        end=$(( (pos + len - 1 - j) & 255 ))
        t=${lst[$start]}; lst[$start]=${lst[$end]}; lst[$end]=$t
      done
      pos=$(( (pos + len + skip) & 255 ))
      skip=$((skip + 1))
    done
  done
  local dense=() xor chunk out="" v
  for ((i=0;i<256;i+=16)); do
    xor=0
    for ((j=0;j<16;j++)); do xor=$(( xor ^ lst[i+j] )); done
    dense+=($xor)
  done
  for v in "${dense[@]}"; do
    printf -v chunk "%08d" "$(bc <<< "obase=2; $v" 2>/dev/null || echo 0)"
    out+="$chunk"
  done
  echo "$out"
}

declare -a grid
used=0
for ((r=0;r<128;r++)); do
  row_hash=$(knot_hash "$key-$r")
  grid[$r]="$row_hash"
  c=${row_hash//0/}
  used=$(( used + ${#c} ))
done
echo "$used"

visited=()
regions=0
inb() { local x=$1 y=$2; (( x>=0 && x<128 && y>=0 && y<128 )); }

getbit() { local r=$1 c=$2; echo "${grid[$r]:$c:1}"; }
isvis() { local r=$1 c=$2; [[ "${visited[$((r*128+c))]:-0}" == 1 ]]; }
setvis() { local r=$1 c=$2; visited[$((r*128+c))]=1; }

for ((i=0;i<128;i++)); do
  for ((j=0;j<128;j++)); do
    if [[ "$(getbit $i $j)" == "1" ]] && ! isvis $i $j; then
      regions=$((regions+1))
      q=("$i,$j"); head=0
      while (( head < ${#q[@]} )); do
        IFS=, read -r x y <<< "${q[$head]}"; head=$((head+1))
        $(! inb $x $y) && continue
        isvis $x $y && continue
        [[ "$(getbit $x $y)" != "1" ]] && continue
        setvis $x $y
        q+=("$((x-1)),$y" "$((x+1)),$y" "$x,$((y-1))" "$x,$((y+1))")
      done
    fi
  done
done
echo "$regions"