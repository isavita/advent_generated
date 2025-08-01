#!/usr/bin/env bash
# Day 20: Race Condition (Part 1) - Bash solution
# Reads input from input.txt and prints the count of cheats that save at least 100 picoseconds.

set -euo pipefail

main() {
  local map srow scol erow ecol rows cols
  map=()
  rows=0
  cols=0
  srow=-1; scol=-1; erow=-1; ecol=-1

  # Read grid
  while IFS= read -r line; do
    [[ -z "$line" ]] && continue
    map+=("$line")
    ((rows++))
    (( cols < ${#line} )) && cols=${#line}
  done < input.txt

  # Normalize lines to equal width
  for ((r=0;r<rows;r++)); do
    local l="${map[r]}"
    if (( ${#l} < cols )); then
      map[r]="$l$(printf '%*s' $((cols - ${#l})) | tr ' ' '#')"
    fi
  done

  # Find S and E
  for ((r=0;r<rows;r++)); do
    line="${map[r]}"
    for ((c=0;c<cols;c++)); do
      ch="${line:c:1}"
      if [[ "$ch" == "S" ]]; then srow=$r; scol=$c; fi
      if [[ "$ch" == "E" ]]; then erow=$r; ecol=$c; fi
    done
  done

  # BFS from S along track to get shortest path distances to E (or all)
  # We need distances from each track cell to E along normal movement.
  # We'll BFS from E to fill dist_to_end for track cells.
  local -a dist
  local total=$((rows*cols))
  dist=()
  for ((i=0;i<total;i++)); do dist[i]=-1; done

  in_bounds() { (( $1>=0 && $1<rows && $2>=0 && $2<cols )); }

  idx() { echo $(( $1*cols + $2 )); }

  is_track() {
    local ch="${map[$1]:$2:1}"
    [[ "$ch" == "." || "$ch" == "S" || "$ch" == "E" ]]
  }

  # BFS queue arrays
  local -a qR qC
  local qh=0 qt=0
  qR=(); qC=()

  # Start at E
  dist[$(idx "$erow" "$ecol")]=0
  qR[qt]=$erow; qC[qt]=$ecol; ((qt++))

  local dr=(1 -1 0 0) dc=(0 0 1 -1)

  while (( qh < qt )); do
    local r="${qR[qh]}" c="${qC[qh]}"
    ((qh++))
    local d="${dist[$(idx "$r" "$c")]}"
    for k in {0..3}; do
      local nr=$((r+dr[k])) nc=$((c+dc[k]))
      if in_bounds "$nr" "$nc" && is_track "$nr" "$nc"; then
        local id=$(idx "$nr" "$nc")
        if (( dist[id] == -1 )); then
          dist[id]=$((d+1))
          qR[qt]=$nr; qC[qt]=$nc; ((qt++))
        fi
      fi
    done
  done

  # Get baseline path length from S to E
  local base="${dist[$(idx "$srow" "$scol")]}"

  # For part 1: a single cheat that lasts up to 2 picoseconds (i.e., 1 or 2 steps through walls)
  # Each cheat has a start track cell A and end track cell B with Manhattan distance <=2 and >0.
  # Cheated path length = dist_start_to_A + cheat_len + dist_to_end_from_B
  # dist_start_to_A along track equals base - dist_to_end_from_A because path is unique.
  # So total = (base - dist[A]) + cheat_len + dist[B] = base + (cheat_len + dist[B] - dist[A])
  # Saved = base - total = dist[A] - dist[B] - cheat_len
  # We count pairs (A,B) with |A-B|_Manhattan in {1,2} and path crossing only through walls during cheat except endpoints.
  # However, rules allow passing through walls for up to 2 steps; endpoints must be track.
  # Any cells in between may be walls or track; that's allowed.
  # Therefore, endpoints must be track; intermediate may be anything.

  # Pre-collect all track cells with valid dist (reachable to E)
  local -a tracksR tracksC
  tracksR=(); tracksC=()
  for ((r=0;r<rows;r++)); do
    for ((c=0;c<cols;c++)); do
      if is_track "$r" "$c"; then
        local id=$(idx "$r" "$c")
        (( dist[id] >= 0 )) && { tracksR+=("$r"); tracksC+=("$c"); }
      fi
    done
  done

  # Build a quick set/map for track check (we already have is_track), no need more.

  # Count cheats saving at least 100
  local threshold=100
  local count=0

  # Precompute neighborhood offsets within manhattan distance <=2 excluding (0,0)
  local offs=()
  for dr0 in -2 -1 0 1 2; do
    for dc0 in -2 -1 0 1 2; do
      local md=$(( dr0<0?-dr0:dr0 ))
      local nd=$(( dc0<0?-dc0:dc0 ))
      local man=$((md+nd))
      if (( man>=1 && man<=2 )); then
        offs+=("$dr0,$dc0,$man")
      fi
    done
  done

  # For uniqueness: cheats identified by start and end positions; we iterate A as start, B as end.
  local n=${#tracksR[@]}
  for ((i=0;i<n;i++)); do
    local ar=${tracksR[i]} ac=${tracksC[i]}
    local aid=$(idx "$ar" "$ac")
    local distA=${dist[aid]}
    # Skip if distA < 0 just in case
    (( distA < 0 )) && continue
    for entry in "${offs[@]}"; do
      IFS=, read -r drr dcc clen <<< "$entry"
      local br=$((ar+drr)) bc=$((ac+dcc))
      if in_bounds "$br" "$bc" && is_track "$br" "$bc"; then
        local bid=$(idx "$br" "$bc")
        local distB=${dist[bid]}
        (( distB < 0 )) && continue
        local saved=$(( distA - distB - clen ))
        if (( saved >= threshold )); then
          ((count++))
        fi
      fi
    done
  done

  echo "$count"
}

main "$@"