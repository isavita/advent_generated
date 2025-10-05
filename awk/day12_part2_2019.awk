#!/usr/bin/awk -f
BEGIN {
  n = 0
  while ((getline line < "input.txt") > 0) {
    tmp = line
    if (tmp ~ /<x=/) {
      gsub(/[^-0-9]+/, " ", tmp)
      split(tmp, a, " ")
      if (a[1] != "" && a[2] != "" && a[3] != "") {
        n++
        posX[n] = a[1] + 0
        posY[n] = a[2] + 0
        posZ[n] = a[3] + 0
        initX[n] = posX[n]
        initY[n] = posY[n]
        initZ[n] = posZ[n]
        velX[n] = 0
        velY[n] = 0
        velZ[n] = 0
      }
    }
  }
  close("input.txt")

  if (n > 0) {
    cycleX = findCycleX()
    cycleY = findCycleY()
    cycleZ = findCycleZ()
    ans = lcm(lcm(cycleX, cycleY), cycleZ)
    print ans
  }
  exit
}
function gcd(a,b) { while (b != 0) { t = b; b = a % b; a = t } return a }
function lcm(a,b) { return (a / gcd(a,b)) * b }

function findCycleX(   i, j, steps, px, vx, initPx) {
  for (i = 1; i <= n; i++) {
    px[i] = posX[i]
    vx[i] = velX[i]
    initPx[i] = initX[i]
  }
  steps = 0
  while (1) {
    steps++
    for (i = 1; i <= n; i++) {
      for (j = i+1; j <= n; j++) {
        if (px[i] > px[j]) { vx[i]--; vx[j]++ } else if (px[i] < px[j]) { vx[i]++; vx[j]-- }
      }
    }
    for (i = 1; i <= n; i++) px[i] += vx[i]
    ok = 1
    for (i = 1; i <= n; i++) { if (px[i] != initPx[i] || vx[i] != 0) { ok = 0; break } }
    if (ok) return steps
  }
}
function findCycleY(   i, j, steps, py, vy, initPy) {
  for (i = 1; i <= n; i++) {
    py[i] = posY[i]
    vy[i] = velY[i]
    initPy[i] = initY[i]
  }
  steps = 0
  while (1) {
    steps++
    for (i = 1; i <= n; i++) {
      for (j = i+1; j <= n; j++) {
        if (py[i] > py[j]) { vy[i]--; vy[j]++ } else if (py[i] < py[j]) { vy[i]++; vy[j]-- }
      }
    }
    for (i = 1; i <= n; i++) py[i] += vy[i]
    ok = 1
    for (i = 1; i <= n; i++) { if (py[i] != initPy[i] || vy[i] != 0) { ok = 0; break } }
    if (ok) return steps
  }
}
function findCycleZ(   i, j, steps, pz, vz, initPz) {
  for (i = 1; i <= n; i++) {
    pz[i] = posZ[i]
    vz[i] = velZ[i]
    initPz[i] = initZ[i]
  }
  steps = 0
  while (1) {
    steps++
    for (i = 1; i <= n; i++) {
      for (j = i+1; j <= n; j++) {
        if (pz[i] > pz[j]) { vz[i]--; vz[j]++ } else if (pz[i] < pz[j]) { vz[i]++; vz[j]-- }
      }
    }
    for (i = 1; i <= n; i++) pz[i] += vz[i]
    ok = 1
    for (i = 1; i <= n; i++) { if (pz[i] != initPz[i] || vz[i] != 0) { ok = 0; break } }
    if (ok) return steps
  }
}