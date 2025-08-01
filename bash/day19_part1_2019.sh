#!/usr/bin/env bash
# Advent of Code 2019 - Day 19 Part 1 (Tractor Beam)
# Reads Intcode from input.txt, counts affected points in 50x50 area.

set -euo pipefail

# Run an Intcode program with provided inputs (space-separated).
# Prints the program's outputs (space-separated) to stdout.
run_intcode() {
  local inputs_str="$1"
  awk -v inputs="$inputs_str" '
  function get(addr) { return mem[addr] }
  function set(addr, val) { mem[addr]=val }
  function raddr(mode, off) { # address for read
    if (mode==0) return get(ip+off)
    if (mode==1) return ip+off
    if (mode==2) return rb + get(ip+off)
  }
  function waddr(mode, off) { # address for write
    if (mode==0) return get(ip+off)
    if (mode==2) return rb + get(ip+off)
    # mode 1 not valid for writes
    return -1
  }
  BEGIN{
    FS=","; OFS=" ";
  }
  {
    for (i=1;i<=NF;i++) mem[i-1]=$i
  }
  END{
    # sparse memory defaults to 0
    for (i in mem) if (mem[i]=="") mem[i]=0
    split(inputs, inq, /[[:space:]]+/); inhead=1; intail=length(inq)
    ip=0; rb=0
    out=""
    while (1) {
      inst = get(ip)+0
      op = inst % 100
      m1 = int(inst/100) % 10
      m2 = int(inst/1000) % 10
      m3 = int(inst/10000) % 10
      if (op==99) break
      if (op==1) { # add
        set(waddr(m3,3), (get(raddr(m1,1))+0) + (get(raddr(m2,2))+0)); ip+=4
      } else if (op==2) { # mul
        set(waddr(m3,3), (get(raddr(m1,1))+0) * (get(raddr(m2,2))+0)); ip+=4
      } else if (op==3) { # input
        if (inhead>intail || inq[inhead]=="") { print "ERR: no input" > "/dev/stderr"; exit 1 }
        set(waddr(m1,1), inq[inhead]+0); inhead++; ip+=2
      } else if (op==4) { # output
        val = get(raddr(m1,1))+0
        if (out=="") out=val; else out=out" "val
        ip+=2
      } else if (op==5) { # jnz
        if ((get(raddr(m1,1))+0)!=0) ip = get(raddr(m2,2))+0; else ip+=3
      } else if (op==6) { # jz
        if ((get(raddr(m1,1))+0)==0) ip = get(raddr(m2,2))+0; else ip+=3
      } else if (op==7) { # lt
        set(waddr(m3,3), ((get(raddr(m1,1))+0) < (get(raddr(m2,2))+0)) ? 1 : 0); ip+=4
      } else if (op==8) { # eq
        set(waddr(m3,3), ((get(raddr(m1,1))+0) == (get(raddr(m2,2))+0)) ? 1 : 0); ip+=4
      } else if (op==9) { # adjust rb
        rb += get(raddr(m1,1))+0; ip+=2
      } else {
        print "ERR: bad opcode " op " at " ip > "/dev/stderr"; exit 1
      }
    }
    if (out!="") print out
  }' input.txt
}

main() {
  if [[ ! -f input.txt ]]; then
    echo "input.txt not found" >&2
    exit 1
  fi

  local total=0
  # Scan 50x50 grid: x=0..49, y=0..49
  for ((y=0; y<50; y++)); do
    for ((x=0; x<50; x++)); do
      # Feed inputs as "x y"
      out="$(run_intcode "$x $y")"
      # Last token is the output we need (usually single value)
      val="${out##* }"
      if [[ "$val" == "1" ]]; then
        ((total++))
      fi
    done
  done

  echo "$total"
}

main "$@"