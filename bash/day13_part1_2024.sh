
#!/bin/bash

solve_machine() {
  local a_x=$(grep "Button A" <<< "$1" | sed 's/.*X+\([0-9]*\),.*/\1/')
  local a_y=$(grep "Button A" <<< "$1" | sed 's/.*Y+\([0-9]*\).*/\1/')
  local b_x=$(grep "Button B" <<< "$1" | sed 's/.*X+\([0-9]*\),.*/\1/')
  local b_y=$(grep "Button B" <<< "$1" | sed 's/.*Y+\([0-9]*\).*/\1/')
  local prize_x=$(grep "Prize" <<< "$1" | sed 's/.*X=\([0-9]*\),.*/\1/')
  local prize_y=$(grep "Prize" <<< "$1" | sed 's/.*Y=\([0-9]*\).*/\1/')

  local min_tokens=999999999
  local found=0

  for a_count in $(seq 0 100); do
    for b_count in $(seq 0 100); do
      local current_x=$((a_count * a_x + b_count * b_x))
      local current_y=$((a_count * a_y + b_count * b_y))

      if [[ "$current_x" -eq "$prize_x" && "$current_y" -eq "$prize_y" ]]; then
        local tokens=$((a_count * 3 + b_count * 1))
        if [[ "$tokens" -lt "$min_tokens" ]]; then
          min_tokens="$tokens"
        fi
        found=1
      fi
    done
  done

  if [[ "$found" -eq 1 ]]; then
    echo "$min_tokens"
  else
    echo "0"
  fi
}

total_tokens=0
prizes_won=0
while IFS= read -r line; do
  if [[ "$line" == "Button A:"* ]]; then
    machine_data="$line"
    IFS= read -r line
    machine_data="$machine_data"$'\n'"$line"
    IFS= read -r line
    machine_data="$machine_data"$'\n'"$line"
    tokens=$(solve_machine "$machine_data")
    if [[ "$tokens" -gt 0 ]]; then
      total_tokens=$((total_tokens + tokens))
      prizes_won=$((prizes_won + 1))
    fi
  fi
done < input.txt

echo "$prizes_won"
echo "$total_tokens"
