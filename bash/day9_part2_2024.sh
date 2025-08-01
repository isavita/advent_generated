#!/usr/bin/env bash
# Advent of Code 2023 Day 9 (Disk Fragmenter) - Part 1 and Part 2
# Reads disk map from input.txt and prints:
# Part 1 checksum (block-by-block compaction) and Part 2 checksum (whole-file compaction).

set -euo pipefail

main() {
  local s
  s="$(tr -d '\n' < input.txt)"

  part1 "$s"
  part2 "$s"
}

# Part 1: move blocks from rightmost file blocks into leftmost free slots
part1() {
  local s="$1"
  awk -v s="$s" '
  function parse() {
    n=split(s,a,"")
    files_n=0
    pos=0
    id=0
    for(i=1;i<=n;i++){
      len=a[i]+0
      if(len==0) continue
      if((i%2)==1){ # file
        files_len[id]=len
        files_pos[id]=pos
        id++
      }
      pos+=len
    }
    files_n=id
    total_pos=pos
  }
  function build_blocks() {
    # Represent disk as arrays: blk_id[p]=fileId or -1 for free
    for(i=0;i<total_pos;i++) blk_id[i]=-1
    for(fid=0;fid<files_n;fid++){
      base=files_pos[fid]
      L=files_len[fid]
      for(k=0;k<L;k++) blk_id[base+k]=fid
    }
  }
  function compact_blocks() {
    left=0
    right=total_pos-1
    # advance left to first free
    while(left<=right && blk_id[left]!=-1) left++
    # move blocks from right into left free slots
    while(left<right){
      while(left<right && blk_id[left]!=-1) left++
      while(left<right && blk_id[right]==-1) right--
      if(left<right){
        blk_id[left]=blk_id[right]
        blk_id[right]=-1
        left++; right--
      }
    }
  }
  function checksum() {
    cs=0
    for(i=0;i<total_pos;i++){
      if(blk_id[i]!=-1) cs+= i*blk_id[i]
    }
    return cs
  }
  BEGIN{
    parse()
    build_blocks()
    compact_blocks()
    print checksum()
  }'
}

# Part 2: move whole files, highest ID to lowest, to leftmost suitable free segment to the left
part2() {
  local s="$1"
  awk -v s="$s" '
  function parse() {
    n=split(s,a,"")
    files_n=0
    pos=0
    id=0
    for(i=1;i<=n;i++){
      len=a[i]+0
      if((i%2)==1){ # file
        files_len[id]=len
        files_pos[id]=pos
        id++
      }
      pos+=len
    }
    files_n=id
    total_pos=pos
  }
  function build_segments() {
    # Build ordered segments alternating file/free based on input string
    segN=0
    cur=0
    id=0
    for(i=1;i<=length(s);i++){
      len=substr(s,i,1)+0
      if(len==0) continue
      seg_len[segN]=len
      seg_start[segN]=cur
      if((i%2)==1){ seg_type[segN]=1; seg_fid[segN]=id; id++ } else { seg_type[segN]=0; seg_fid[segN]=-1 }
      cur+=len
      segN++
    }
  }
  function find_free_before(pos, need,    j,best_idx,best_start) {
    best_idx=-1
    best_start=-1
    for(j=0;j<segN;j++){
      if(seg_type[j]!=0) continue
      if(seg_start[j] >= pos) break
      if(seg_len[j] >= need){
        if(best_start==-1 || seg_start[j] < best_start){
          best_start=seg_start[j]
          best_idx=j
        }
      }
    }
    return best_idx
  }
  function move_file(fid,    i,j,need,fpos,free_i,fs,fl) {
    # locate file segment index
    for(i=0;i<segN;i++){
      if(seg_type[i]==1 && seg_fid[i]==fid){
        need=seg_len[i]
        fpos=seg_start[i]
        break
      }
    }
    # find leftmost suitable free segment to the left
    free_i=find_free_before(fpos, need)
    if(free_i==-1) return
    # Move file into start of free segment (split free if needed), then free original spot
    fs=seg_start[free_i]; fl=seg_len[free_i]

    # Case: free before file index might shift due to insertions; handle by operations carefully.

    # 1) If free has leading remainder
    if(fl>need){
      # split free into [fs,need] and [fs+need, fl-need]
      # replace free_i with occupied by fid, insert new free right after
      # shift arrays to make room if needed
      # Perform insertion by moving tail one step right
      for(j=segN;j>free_i;j--){
        seg_type[j]=seg_type[j-1]; seg_len[j]=seg_len[j-1]; seg_start[j]=seg_start[j-1]; seg_fid[j]=seg_fid[j-1]
      }
      segN++
      # Now free_i holds original free; we convert it to file, and free_i+1 is the trailing free
      seg_type[free_i]=1; seg_fid[free_i]=fid; seg_len[free_i]=need; seg_start[free_i]=fs
      seg_type[free_i+1]=0; seg_fid[free_i+1]=-1; seg_len[free_i+1]=fl-need; seg_start[free_i+1]=fs+need
      # Update starts of subsequent segments by +0 (no net length change before them), so only inserts shift indices, not starts
    } else {
      # fl == need, turn this free segment into the file
      seg_type[free_i]=1; seg_fid[free_i]=fid; seg_len[free_i]=need
      # seg_start unchanged
    }

    # 2) Remove original file segment: turn it into free and coalesce neighbors if adjacent frees
    # Find current index of the original file segment (it may have shifted):
    idx=-1
    for(j=0;j<segN;j++){
      if(seg_type[j]==1 && seg_fid[j]==fid && seg_start[j]==fs) { # this is the moved-to segment
        continue
      }
      if(seg_type[j]==1 && seg_fid[j]==fid){
        idx=j; break
      }
    }
    if(idx==-1) return
    seg_type[idx]=0; seg_fid[idx]=-1
    # Coalesce with left
    if(idx>0 && seg_type[idx-1]==0){
      seg_len[idx-1]+=seg_len[idx]
      # remove idx
      for(j=idx;j<segN-1;j++){
        seg_type[j]=seg_type[j+1]; seg_len[j]=seg_len[j+1]; seg_start[j]=seg_start[j+1]; seg_fid[j]=seg_fid[j+1]
      }
      segN--; idx--
    }
    # Coalesce with right
    if(idx<segN-1 && seg_type[idx+1]==0){
      seg_len[idx]+=seg_len[idx+1]
      # remove idx+1
      for(j=idx+1;j<segN-1;j++){
        seg_type[j]=seg_type[j+1]; seg_len[j]=seg_len[j+1]; seg_start[j]=seg_start[j+1]; seg_fid[j]=seg_fid[j+1]
      }
      segN--
    }

    # Recompute starts from left to maintain correctness
    seg_start[0]=0
    for(j=1;j<segN;j++){
      seg_start[j]=seg_start[j-1]+seg_len[j-1]
    }
  }
  function checksum() {
    # iterate segments and accumulate pos*fid for file segments
    cs=0
    pos=0
    for(i=0;i<segN;i++){
      if(seg_type[i]==1){
        fid=seg_fid[i]
        L=seg_len[i]
        # sum of (pos..pos+L-1) * fid
        # arithmetic series sum of positions
        # sum = fid * (L*pos + L*(L-1)/2)
        cs += fid * ( L*pos + (L*(L-1))/2 )
      }
      pos += seg_len[i]
    }
    return cs
  }
  BEGIN{
    parse()
    build_segments()
    # Move files from highest ID to 0
    for(fid=files_n-1; fid>=0; fid--){
      move_file(fid)
    }
    print checksum()
  }'
}

main "$@"