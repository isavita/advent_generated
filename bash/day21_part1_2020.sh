#!/usr/bin/env bash
# Advent of Code 2020 Day 21 - Part 1: Allergen Assessment
# Reads from input.txt and prints the count of appearances of ingredients
# that cannot possibly contain any listed allergen.

set -euo pipefail

main() {
  local file="input.txt"
  [[ -f "$file" ]] || { echo "input.txt not found" >&2; exit 1; }

  awk '
    function split_set(str, arr,    n,i) {
      n = split(str, arr, /[ ,]+/)
      # remove empty tokens potentially caused by regex
      for (i in arr) if (arr[i] == "") delete arr[i]
      return n
    }
    function set_intersection(a, b, out,    k) {
      delete out
      for (k in a) if (k in b) out[k]=1
    }
    function set_union(a, b, out,    k) {
      delete out
      for (k in a) out[k]=1
      for (k in b) out[k]=1
    }
    function copy_set(src, dst,    k){ delete dst; for (k in src) dst[k]=1 }
    function count_set(s,    k,c){ c=0; for (k in s) c++; return c }

    BEGIN {
      FS=""
    }

    {
      line=$0
      # Parse ingredients and allergens
      # Format: "ing ing ing (contains a, b, c)" OR just "ing ing"
      ing_part=line
      alg_part=""
      if (match(line, /\(contains /)) {
        ing_part = substr(line, 1, RSTART-2)
        alg_part = substr(line, RSTART+10)  # after "(contains "
        sub(/\)$/, "", alg_part)
      }

      # Tokenize ingredients
      n_ing = split(ing_part, tmp_ing, /[ ]+/)
      delete ings
      for (i=1;i<=n_ing;i++){
        if (tmp_ing[i]=="") continue
        ings[tmp_ing[i]] = 1
        ing_count[tmp_ing[i]]++
      }

      # Tokenize allergens
      delete algs
      if (alg_part != "") {
        n_alg = split(alg_part, tmp_alg, /[ ,]+/)
        for (i=1;i<=n_alg;i++){
          if (tmp_alg[i]=="") continue
          algs[tmp_alg[i]] = 1
          allergens_seen[tmp_alg[i]] = 1
        }
      }

      # For each allergen, intersect candidate set with current ingredients
      for (a in algs) {
        # Build a temp set for current line ingredients
        delete curr
        for (k in ings) curr[k]=1

        if (!(a in init_done)) {
          # initialize candidates[a] = current ingredients
          for (k in curr) candidates[a SUBSEP k]=1
          init_done[a]=1
        } else {
          # intersect existing candidates with curr
          # existing: entries in candidates with key (a,k)
          delete newset
          for (ck in candidates) {
            split(ck, parts, SUBSEP)
            if (parts[1] != a) continue
            k = parts[2]
            if (k in curr) newset[a SUBSEP k]=1
          }
          # replace with intersection
          # first, delete all old (a,*) then set new ones
          for (ck in candidates) {
            split(ck, parts, SUBSEP)
            if (parts[1] == a) delete candidates[ck]
          }
          for (ck in newset) candidates[ck]=1
        }
      }
    }

    END {
      # Build union of all allergen candidate ingredients
      delete bad
      for (ck in candidates) {
        split(ck, parts, SUBSEP)
        ing = parts[2]
        bad[ing]=1
      }

      # Sum counts of ingredients not in bad
      total=0
      for (ing in ing_count) {
        if (!(ing in bad)) total += ing_count[ing]
      }
      print total
    }
  ' "$file"
}

main "$@"