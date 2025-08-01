#!/usr/bin/env bash
# AoC 2022 Day 11 - Monkey in the Middle
# Reads from input.txt and prints two answers:
# Part 1: monkey business after 20 rounds with /3 relief
# Part 2: monkey business after 10000 rounds with modulo relief
# Portable bash-only solution.

set -euo pipefail

main() {
  local input="input.txt"
  awk -v R1=20 -v R2=10000 '
    function trim(s){ sub(/^[ \t\r\n]+/,"",s); sub(/[ \t\r\n]+$/,"",s); return s }
    function push(arr, idx, val){ arr[idx, ++len[idx]] = val }
    function clear_queue(idx){ len[idx]=0 }
    function op_apply(old, idx,    a,b) {
      a = opA[idx]; b = opB[idx]
      if (a=="old") a = old; else a += 0
      if (b=="old") b = old; else b += 0
      if (op[idx]=="+") return a + b
      else return a * b
    }
    function lcm_mod(x, m){ return x % m }

    BEGIN{
      n=0
    }
    # Parse input blocks
    /^Monkey[ \t]/{
      split($2, t, ":"); midx=t[1]+0
      if (midx+0 != n) n = midx+1
      next
    }
    /Starting items:/{
      gsub(/Starting items:[ \t]*/,"")
      gsub(/,/,"")
      split($0, vals, /[ \t]+/)
      i = midx
      clear_queue(i)
      for (k=1;k<=length(vals);k++) if(vals[k]!="") push(items, i, vals[k]+0)
      next
    }
    /Operation: new =/{
      # Format: Operation: new = <A> <op> <B>
      # A or B can be "old" or number; op is + or *
      split($0, parts, "=")
      s = trim(parts[2])
      split(s, w, /[ \t]+/)
      opA[midx]=w[1]
      op[midx]=w[2]
      opB[midx]=w[3]
      next
    }
    /Test: divisible by/{
      test[midx] = $(NF)+0
      next
    }
    /If true:/{
      trueTo[midx] = $(NF)+0
      next
    }
    /If false:/{
      falseTo[midx] = $(NF)+0
      next
    }
    END{
      # Precompute modulus for part 2: product of all divisors (they are prime in puzzle, product works as lcm)
      MOD=1
      for (i=0;i<n;i++) MOD *= test[i]

      # Copy initial state
      for (i=0;i<n;i++){
        init_len[i]=len[i]
        for (j=1;j<=len[i];j++) init_items[i,j]=items[i,j]
      }

      # Run simulation helper: rounds, relief_mode (1 for /3, 2 for mod)
      simulate(R1, 1)
      ans1 = top2product()

      # Restore state
      for (i=0;i<n;i++){
        len[i]=init_len[i]
        inspected[i]=0
        for (j=1;j<=len[i];j++) items[i,j]=init_items[i,j]
      }

      simulate(R2, 2)
      ans2 = top2product()

      print ans1
      print ans2
    }

    function simulate(R, mode,    r,i,j,w,neww,d,to) {
      for (r=1;r<=R;r++){
        for (i=0;i<n;i++){
          for (j=1;j<=len[i];j++){
            w = items[i,j]+0
            inspected[i]++
            neww = op_apply(w, i)
            if (mode==1){
              neww = int(neww/3)
            } else {
              # keep manageable using modulus of product of divisors
              neww = neww % MOD
            }
            d = test[i]
            to = (neww % d == 0) ? trueTo[i] : falseTo[i]
            push(items, to, neww)
          }
          clear_queue(i)
        }
      }
    }

    function top2product(    a,b,i){
      a=0; b=0
      for (i=0;i<n;i++){
        if (inspected[i]>a){ b=a; a=inspected[i] }
        else if (inspected[i]>b){ b=inspected[i] }
      }
      return a*b
    }
  ' "$input"
}

main "$@"