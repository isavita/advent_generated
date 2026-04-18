
#!/usr/bin/env bash
set -euo pipefail

main() {
  local -a nums=()
  while IFS= read -r line; do
    [[ -n "$line" ]] && nums+=("$line")
  done < input.txt

  python3 - "${nums[@]}" <<'PY'
import sys

MOD = 16777216
STEPS = 2000

def next_secret(x):
    x = ((x << 6) ^ x) & (MOD - 1)
    x = ((x >> 5) ^ x) & (MOD - 1)
    x = ((x << 11) ^ x) & (MOD - 1)
    return x

buyers = list(map(int, sys.argv[1:]))

# Part 1
part1 = 0

# Part 2: total bananas for each 4-change sequence
totals = {}

for start in buyers:
    x = start
    prev_price = x % 10
    diffs = []
    first_seen = {}

    for _ in range(STEPS):
        x = next_secret(x)
        price = x % 10
        diff = price - prev_price
        prev_price = price
        diffs.append(diff)

        if len(diffs) >= 4:
            key = tuple(diffs[-4:])
            if key not in first_seen:
                first_seen[key] = price

    part1 += x

    for k, v in first_seen.items():
        totals[k] = totals.get(k, 0) + v

part2 = max(totals.values()) if totals else 0

print(part1)
print(part2)
PY
}

main "$@"
