
#!/usr/bin/env bash

awk '
BEGIN { parsing=1 }
{
  if ($0 ~ /^[[:space:]]*$/) { parsing=0; next }
  if (parsing) {
    split($0, a, "-")
    mins[++n] = a[1] + 0
    maxs[n] = a[2] + 0
  } else {
    id = $0 + 0
    for (i = 1; i <= n; i++) {
      if (id >= mins[i] && id <= maxs[i]) {
        fresh++
        break
      }
    }
  }
}
END { printf "Number of fresh ingredients: %d\n", fresh }
' input.txt
