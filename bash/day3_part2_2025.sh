
#!/bin/bash
{
  echo "s=0"
  while read -r line || [[ -n "$line" ]]; do
    s=${line//[[:space:]]/}
    [[ ${#s} -lt 12 ]] && continue
    rem=$(( ${#s} - 12 ))
    stack=""
    for (( i=0; i<${#s}; i++ )); do
      char=${s:i:1}
      while (( rem > 0 )) && [[ -n "$stack" && "${stack: -1}" < "$char" ]]; do
        stack=${stack%?}
        ((rem--))
      done
      stack+="$char"
    done
    echo "s+=${stack:0:12}"
  done < input.txt
  echo "s"
} | bc | sed 's/^/Total output joltage: /'
