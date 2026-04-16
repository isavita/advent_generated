
#!/usr/bin/env bash
set -euo pipefail

key=$(<input.txt)
key=${key%$'\n'}

python3 - <<'PY' "$key"
import sys, hashlib

key = sys.argv[1].encode()

def find(prefix_zeros):
    target = b"\x00" * (prefix_zeros // 2)
    extra = prefix_zeros % 2
    n = 1
    while True:
        h = hashlib.md5(key + str(n).encode()).digest()
        if extra == 0:
            if h.startswith(target):
                return n
        else:
            if h.startswith(target) and (h[prefix_zeros // 2] >> 4) == 0:
                return n
        n += 1

print(find(5))
print(find(6))
PY
