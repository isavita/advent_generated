#!/usr/bin/awk -f
BEGIN {
  fname = "input.txt"
  if ((getline line < fname) <= 0) exit
  close(fname)

  n = split(line, toks, ",")
  for (i = 1; i <= n; i++) code[i-1] = toks[i] + 0

  ip = 0
  relativeBase = 0
  inHead = 0
  inTail = 0
  outHead = 0
  outTail = 0
  halted = 0

  lines[1] = "78 79 84 32 65 32 74 10"
  lines[2] = "78 79 84 32 66 32 84 10"
  lines[3] = "79 82 32 84 32 74 10"
  lines[4] = "78 79 84 32 67 32 84 10"
  lines[5] = "79 82 32 84 32 74 10"
  lines[6] = "65 78 68 32 68 32 74 10"
  lines[7] = "78 79 84 32 65 32 84 10"
  lines[8] = "65 78 68 32 65 32 84 10"
  lines[9] = "79 82 32 69 32 84 10"
  lines[10] = "79 82 32 72 32 84 10"
  lines[11] = "65 78 68 32 84 32 74 10"
  lines[12] = "82 85 78 10"

  for (i = 1; i <= 12; i++) push_line(lines[i])

  vm_run()

  for (i = 0; i < outTail; i++) {
    c = outBuf[i]
    if (c > 127) {
      printf "%d\n", c
      exit
    }
  }
  exit
}

function push_line(str, arr, m, j) {
  m = split(str, arr, /[ \t]+/)
  for (j = 1; j <= m; j++) inputBuf[inTail++] = arr[j] + 0
}

function vm_run(    instruction, opcode, mode1, mode2, mode3, p1, p2, addr, val) {
  while (!halted) {
    instruction = code[ip] + 0
    opcode = instruction % 100
    mode1 = int(instruction / 100) % 10
    mode2 = int(instruction / 1000) % 10
    mode3 = int(instruction / 10000) % 10

    if (opcode == 1) {
      p1 = param_val(ip+1, mode1)
      p2 = param_val(ip+2, mode2)
      addr = param_addr(ip+3, mode3)
      code[addr] = p1 + p2
      ip += 4
    } else if (opcode == 2) {
      p1 = param_val(ip+1, mode1)
      p2 = param_val(ip+2, mode2)
      addr = param_addr(ip+3, mode3)
      code[addr] = p1 * p2
      ip += 4
    } else if (opcode == 3) {
      addr = param_addr(ip+1, mode1)
      if (inHead == inTail) { halted = 1; break }
      code[addr] = inputBuf[inHead++]
      ip += 2
    } else if (opcode == 4) {
      val = param_val(ip+1, mode1)
      outBuf[outTail++] = val
      ip += 2
    } else if (opcode == 5) {
      p1 = param_val(ip+1, mode1)
      p2 = param_val(ip+2, mode2)
      if (p1 != 0) ip = p2
      else ip += 3
    } else if (opcode == 6) {
      p1 = param_val(ip+1, mode1)
      p2 = param_val(ip+2, mode2)
      if (p1 == 0) ip = p2
      else ip += 3
    } else if (opcode == 7) {
      p1 = param_val(ip+1, mode1)
      p2 = param_val(ip+2, mode2)
      addr = param_addr(ip+3, mode3)
      code[addr] = (p1 < p2) ? 1 : 0
      ip += 4
    } else if (opcode == 8) {
      p1 = param_val(ip+1, mode1)
      p2 = param_val(ip+2, mode2)
      addr = param_addr(ip+3, mode3)
      code[addr] = (p1 == p2) ? 1 : 0
      ip += 4
    } else if (opcode == 9) {
      p1 = param_val(ip+1, mode1)
      relativeBase += p1
      ip += 2
    } else if (opcode == 99) {
      halted = 1
    } else {
      print "Invalid opcode: " opcode > "/dev/stderr"
      exit
    }
  }
}

function param_addr(pos, mode, addr) {
  if (mode == 0) {
    addr = code[pos] + 0
  } else if (mode == 1) {
    addr = pos
  } else if (mode == 2) {
    addr = code[pos] + relativeBase
  } else {
    print "Invalid parameter mode: " mode > "/dev/stderr"
    exit
  }
  return addr
}

function param_val(pos, mode) {
  address = param_addr(pos, mode)
  return code[address] + 0
}