#!/bin/bash

read -r line < input.txt
IFS=',' read -r -a program <<< "$line"

input=5
output=0
i=0

getValue() {
  if [[ $2 -eq 0 ]]; then
    echo "${program[program[$1]]}"
  else
    echo "${program[$1]}"
  fi
}

while true; do
  opcode=$((program[i] % 100))
  modes=$((program[i] / 100))
  param1Mode=$((modes % 10))
  modes=$((modes / 10))
  param2Mode=$((modes % 10))
  
  case $opcode in
    1)
      p1=$(getValue $((i+1)) $param1Mode)
      p2=$(getValue $((i+2)) $param2Mode)
      p3=${program[$((i+3))]}
      program[$p3]=$((p1 + p2))
      ((i+=4))
      ;;
    2)
      p1=$(getValue $((i+1)) $param1Mode)
      p2=$(getValue $((i+2)) $param2Mode)
      p3=${program[$((i+3))]}
      program[$p3]=$((p1 * p2))
      ((i+=4))
      ;;
    3)
      program[program[$((i+1))]]=$input
      ((i+=2))
      ;;
    4)
      output=$(getValue $((i+1)) $param1Mode)
      echo $output
      ((i+=2))
      ;;
    5)
      p1=$(getValue $((i+1)) $param1Mode)
      p2=$(getValue $((i+2)) $param2Mode)
      if [[ $p1 -ne 0 ]]; then
        i=$p2
      else
        ((i+=3))
      fi
      ;;
    6)
      p1=$(getValue $((i+1)) $param1Mode)
      p2=$(getValue $((i+2)) $param2Mode)
      if [[ $p1 -eq 0 ]]; then
        i=$p2
      else
        ((i+=3))
      fi
      ;;
    7)
      p1=$(getValue $((i+1)) $param1Mode)
      p2=$(getValue $((i+2)) $param2Mode)
      p3=${program[$((i+3))]}
      if [[ $p1 -lt $p2 ]]; then
        program[$p3]=1
      else
        program[$p3]=0
      fi
      ((i+=4))
      ;;
    8)
      p1=$(getValue $((i+1)) $param1Mode)
      p2=$(getValue $((i+2)) $param2Mode)
      p3=${program[$((i+3))]}
      if [[ $p1 -eq $p2 ]]; then
        program[$p3]=1
      else
        program[$p3]=0
      fi
      ((i+=4))
      ;;
    99)
      exit 0
      ;;
    *)
      echo "Invalid opcode"
      exit 1
      ;;
  esac
done