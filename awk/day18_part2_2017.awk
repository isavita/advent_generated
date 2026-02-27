#!/usr/bin/awk -f
BEGIN {
  i0 = 1
  i1 = 1
  head0 = 1; tail0 = 0
  head1 = 1; tail1 = 0
  registers0["p"] = 0
  registers1["p"] = 1
  sendCount1 = 0
  idx = 0
  while ((getline line < "input.txt") > 0) {
    n = split(line, t, " ")
    for (k = 1; k <= n; k++) instr[idx+1, k] = t[k]
    idx++
  }
  N = idx
}
function val0(x) {
  return (x ~ /^-?[0-9]+$/) ? (x + 0) : (registers0[x] + 0)
}
function val1(x) {
  return (x ~ /^-?[0-9]+$/) ? (x + 0) : (registers1[x] + 0)
}
END {
  i0 = 1
  i1 = 1
  head0 = 1; tail0 = 0
  head1 = 1; tail1 = 0
  do {
    dead0 = 1
    dead1 = 1
    while (i0 <= N) {
      op = instr[i0, 1]
      if (op == "snd") { tail1++; queue1[tail1] = val0(instr[i0, 2]); dead0 = 0 }
      else if (op == "set") { registers0[instr[i0, 2]] = val0(instr[i0, 3]); dead0 = 0 }
      else if (op == "add") { registers0[instr[i0, 2]] = registers0[instr[i0, 2]] + val0(instr[i0, 3]); dead0 = 0 }
      else if (op == "mul") { registers0[instr[i0, 2]] = registers0[instr[i0, 2]] * val0(instr[i0, 3]); dead0 = 0 }
      else if (op == "mod") { registers0[instr[i0, 2]] = registers0[instr[i0, 2]] % val0(instr[i0, 3]); dead0 = 0 }
      else if (op == "rcv") {
        if (head0 > tail0) { break }
        registers0[instr[i0, 2]] = queue0[head0]; head0++; dead0 = 0
      }
      else if (op == "jgz") {
        if (val0(instr[i0, 2]) > 0) { i0 += val0(instr[i0, 3]) - 1 }
      }
      i0++
    }
    while (i1 <= N) {
      op = instr[i1, 1]
      if (op == "snd") { tail0++; queue0[tail0] = val1(instr[i1, 2]); sendCount1++; dead1 = 0 }
      else if (op == "set") { registers1[instr[i1, 2]] = val1(instr[i1, 3]); dead1 = 0 }
      else if (op == "add") { registers1[instr[i1, 2]] = registers1[instr[i1, 2]] + val1(instr[i1, 3]); dead1 = 0 }
      else if (op == "mul") { registers1[instr[i1, 2]] = registers1[instr[i1, 2]] * val1(instr[i1, 3]); dead1 = 0 }
      else if (op == "mod") { registers1[instr[i1, 2]] = registers1[instr[i1, 2]] % val1(instr[i1, 3]); dead1 = 0 }
      else if (op == "rcv") {
        if (head1 > tail1) { break }
        registers1[instr[i1, 2]] = queue1[head1]; head1++; dead1 = 0
      }
      else if (op == "jgz") {
        if (val1(instr[i1, 2]) > 0) { i1 += val1(instr[i1, 3]) - 1 }
      }
      i1++
    }
  } while (!(dead0 && dead1))
  print sendCount1
}