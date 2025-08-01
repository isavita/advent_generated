
#!/usr/bin/env bash
# AoC 2023 Day 13 - Point of Incidence
# Reads input from input.txt, prints two numbers: part1 and part2 results.

set -euo pipefail

main() {
  local input="input.txt"
  awk -v PART=1 -f - "$input" <<'AWK'
function reset_block(   i){
  rows_n=0
}
function push_row(s){ rows[++rows_n]=s }
function columns_match(i,j,   r){ for(r=1;r<=rows_n;r++) if(substr(rows[r],i,1)!=substr(rows[r],j,1)) return 0; return 1 }
function rows_match(i,j,   c){ for(c=1;c<=cols;c++) if(substr(rows[i],c,1)!=substr(rows[j],c,1)) return 0; return 1 }
function columns_mismatch_count(i,j,   r,cnt){ cnt=0; for(r=1;r<=rows_n;r++) if(substr(rows[r],i,1)!=substr(rows[r],j,1)) cnt++; return cnt }
function rows_mismatch_count(i,j,   c,cnt){ cnt=0; for(c=1;c<=cols;c++) if(substr(rows[i],c,1)!=substr(rows[j],c,1)) cnt++; return cnt }

function score_for_smudge(target_mismatches,   i,d,ok,mism,sc,found,try){
  found=0; sc=0
  # Vertical mirrors between columns i and i+1
  for(i=1;i<cols;i++){
    mism=0
    for(d=0; i-d>=1 && i+1+d<=cols; d++){
      mism += columns_mismatch_count(i-d, i+1+d)
      if(mism>target_mismatches) break
    }
    if(mism==target_mismatches){
      sc += i
      found=1
    }
  }
  # Horizontal mirrors between rows i and i+1
  for(i=1;i<rows_n;i++){
    mism=0
    for(d=0; i-d>=1 && i+1+d<=rows_n; d++){
      mism += rows_mismatch_count(i-d, i+1+d)
      if(mism>target_mismatches) break
    }
    if(mism==target_mismatches){
      sc += 100*i
      found=1
    }
  }
  return sc
}

BEGIN{
  reset_block()
}
# Accumulate blocks separated by blank lines
{
  if($0~/^[[:space:]]*$/){
    if(rows_n>0){
      cols=length(rows[1])
      part1 += score_for_smudge(0)
      part2 += score_for_smudge(1)
      reset_block()
    }
  } else {
    push_row($0)
  }
}
END{
  if(rows_n>0){
    cols=length(rows[1])
    part1 += score_for_smudge(0)
    part2 += score_for_smudge(1)
  }
  print part1
  print part2
}
AWK
}

main "$@"
