#!/usr/bin/env bash
python3 - <<'PY'
import itertools

dist = {}
with open('input.txt') as f:
    for line in f:
        a, _, b, _, d = line.split()
        d = int(d)
        dist.setdefault(a, {})[b] = d
        dist.setdefault(b, {})[a] = d

locations = list(dist.keys())
min_dist = None
for perm in itertools.permutations(locations):
    total = sum(dist[perm[i]][perm[i + 1]] for i in range(len(perm) - 1))
    if min_dist is None or total < min_dist:
        min_dist = total

print(min_dist)
PY