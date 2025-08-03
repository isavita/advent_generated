#!/usr/bin/env bash
set -euo pipefail

awk '
function rev(s,  r,i) {r=""; for(i=length(s); i>0; i--) r=r substr(s,i,1); return r}
function add_border(bstr, tile_id, side,   key) {
  key=bstr
  if(!(key in count)) count[key]=0
  count[key]++
  entries_n++; entries_bstr[entries_n]=key; entries_tid[entries_n]=tile_id; entries_side[entries_n]=side
}
function flush_tile(   r,c,top,bot,left,right) {
  if(tile_id==0 || row!=10) return
  tiles[++tiles_n]=tile_id
  # top/bottom
  top=grid[1]; bot=grid[10]
  # left/right
  left=""; right=""
  for(r=1;r<=10;r++){ left=left substr(grid[r],1,1); right=right substr(grid[r],10,1) }
  add_border(top, tile_id, 0); add_border(rev(top), tile_id, 0)
  add_border(bot, tile_id, 1); add_border(rev(bot), tile_id, 1)
  add_border(left,tile_id, 2); add_border(rev(left),tile_id, 2)
  add_border(right,tile_id,3); add_border(rev(right),tile_id,3)
  row=0
}
BEGIN{
  FS=""
  tiles_n=0; entries_n=0; row=0; tile_id=0
}
{
  if($0 ~ /^Tile [0-9]+:/){
    split($0,a,/[ :]/); flush_tile()
    tile_id = a[2]+0; next
  }
  if(length($0)==0){ flush_tile(); next }
  row++; grid[row]=$0
}
END{
  flush_tile()
  for(i=1;i<=entries_n;i++){
    b=entries_bstr[i]; tid=entries_tid[i]; side=entries_side[i]
    # map border match to original border uniqueness per tile
    # unique if total count of this string is 1
    uniq = (count[b]==1)?1:0
    key = tid ":" side
    # mark unique if seen unique; if any side seen as matched, keep non-unique
    if(!(key in uniq_side)) uniq_side[key]=uniq
    else if(uniq_side[key]==1 && uniq==0) uniq_side[key]=0
  }
  result=1; corner_count=0
  for(ti=1; ti<=tiles_n; ti++){
    tid=tiles[ti]
    ue=0
    for(s=0;s<4;s++){
      k=tid ":" s
      if((k in uniq_side) && uniq_side[k]==1) ue++
    }
    if(ue==2){ result *= tid; corner_count++ }
  }
  printf "%d\n", result
}
' input.txt