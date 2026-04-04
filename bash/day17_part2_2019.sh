
#!/usr/bin/env bash
set -euo pipefail

awk '
BEGIN {
  FS = ","
  getline line < "input.txt"
  n = split(line, init, ",")
  for (i = 0; i < 256; i++) ord[sprintf("%c", i)] = i
  dx[0]=0; dy[0]=-1; dx[1]=1; dy[1]=0; dx[2]=0; dy[2]=1; dx[3]=-1; dy[3]=0

  reset_machine()
  run_intcode()

  x = y = 0
  for (i = 1; i <= out_ptr; i++) {
    c = sprintf("%c", outq[i])
    if (c == "\n") { y++; x = 0; continue }
    if (c ~ /[\^v<>]/) {
      rx = x; ry = y
      rd = (c == "^" ? 0 : (c == ">" ? 1 : (c == "v" ? 2 : 3)))
    }
    if (c != ".") grid[x "," y] = 1
    x++
  }

  while (1) {
    step = 0
    while (grid[(rx + dx[rd]) "," (ry + dy[rd])]) {
      rx += dx[rd]; ry += dy[rd]; step++
    }
    if (step > 0) path = path step ","
    nr = (rd + 1) % 4
    nl = (rd + 3) % 4
    if (grid[(rx + dx[nr]) "," (ry + dy[nr])]) { path = path "R,"; rd = nr }
    else if (grid[(rx + dx[nl]) "," (ry + dy[nl])]) { path = path "L,"; rd = nl }
    else break
  }

  for (la = 2; la <= 21; la++) {
    sa = substr(path, 1, la)
    if (substr(sa, la, 1) != ",") continue
    rem1 = path
    while (substr(rem1, 1, la) == sa) rem1 = substr(rem1, la + 1)
    for (lb = 2; lb <= 21; lb++) {
      sb = substr(rem1, 1, lb)
      if (substr(sb, lb, 1) != ",") continue
      rem2 = path
      while (1) {
        if (substr(rem2, 1, la) == sa) rem2 = substr(rem2, la + 1)
        else if (substr(rem2, 1, lb) == sb) rem2 = substr(rem2, lb + 1)
        else break
      }
      for (lc = 2; lc <= 21; lc++) {
        sc = substr(rem2, 1, lc)
        if (substr(sc, lc, 1) != ",") continue
        main = check(path, sa, sb, sc)
        if (main != "" && length(main) <= 21) { found = 1; break }
      }
      if (found) break
    }
    if (found) break
  }

  reset_machine()
  mem[0] = 2
  add_input(substr(main, 1, length(main) - 1) "\n")
  add_input(substr(sa, 1, length(sa) - 1) "\n")
  add_input(substr(sb, 1, length(sb) - 1) "\n")
  add_input(substr(sc, 1, length(sc) - 1) "\n")
  add_input("n\n")
  run_intcode()

  print last_out
}

function reset_machine(i) {
  delete mem
  for (i = 1; i <= n; i++) mem[i - 1] = init[i]
  ip = rb = out_ptr = in_max = 0
  in_ptr = 1
}

function add_input(str, i, l) {
  l = length(str)
  for (i = 1; i <= l; i++) inq[++in_max] = ord[substr(str, i, 1)]
}

function check(p, a, b, c, s, main, found, la, lb, lc) {
  s = p
  main = ""
  la = length(a); lb = length(b); lc = length(c)
  while (s != "") {
    found = 0
    if (la > 0 && substr(s, 1, la) == a) { s = substr(s, la + 1); main = main "A,"; found = 1 }
    else if (lb > 0 && substr(s, 1, lb) == b) { s = substr(s, lb + 1); main = main "B,"; found = 1 }
    else if (lc > 0 && substr(s, 1, lc) == c) { s = substr(s, lc + 1); main = main "C,"; found = 1 }
    if (!found) return ""
  }
  return main
}

function run_intcode(v, op, m1, m2, m3, p1, p2, p3, v1, v2, addr) {
  while (1) {
    v = mem[ip]
    op = v % 100
    m1 = int(v / 100) % 10
    m2 = int(v / 1000) % 10
    m3 = int(v / 10000) % 10
    p1 = mem[ip + 1]
    p2 = mem[ip + 2]
    p3 = mem[ip + 3]
    v1 = (m1 == 1 ? p1 : (m1 == 2 ? mem[rb + p1] : mem[p1])) + 0
    v2 = (m2 == 1 ? p2 : (m2 == 2 ? mem[rb + p2] : mem[p2])) + 0

    if (op == 99) break
    else if (op == 1) { addr = (m3 == 2 ? rb + p3 : p3); mem[addr] = v1 + v2; ip += 4 }
    else if (op == 2) { addr = (m3 == 2 ? rb + p3 : p3); mem[addr] = v1 * v2; ip += 4 }
    else if (op == 3) {
      if (in_ptr > in_max) return
      addr = (m1 == 2 ? rb + p1 : p1)
      mem[addr] = inq[in_ptr++]
      ip += 2
    }
    else if (op == 4) { last_out = v1; outq[++out_ptr] = v1; ip += 2 }
    else if (op == 5) { ip = (v1 != 0 ? v2 : ip + 3) }
    else if (op == 6) { ip = (v1 == 0 ? v2 : ip + 3) }
    else if (op == 7) { addr = (m3 == 2 ? rb + p3 : p3); mem[addr] = (v1 < v2 ? 1 : 0); ip += 4 }
    else if (op == 8) { addr = (m3 == 2 ? rb + p3 : p3); mem[addr] = (v1 == v2 ? 1 : 0); ip += 4 }
    else if (op == 9) { rb += v1; ip += 2 }
  }
}
' </dev/null
