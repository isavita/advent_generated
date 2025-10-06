BEGIN {
  fname = "input.txt"
  min = 200000000000000
  max = 400000000000000
  n = 0
  while ((getline line < fname) > 0) {
    if (line ~ /^[ \t]*$/) continue
    tmp = line
    gsub(/,/, " ", tmp)
    gsub(/@/, " ", tmp)
    split(tmp, a, /[ \t]+/)
    if (length(a) >= 6) {
      n++
      posx[n] = a[1] + 0
      posy[n] = a[2] + 0
      posz[n] = a[3] + 0
      velx[n] = a[4] + 0
      vely[n] = a[5] + 0
      velz[n] = a[6] + 0
    }
  }
  close(fname)
}
END {
  count = 0
  for (i = 2; i <= n; i++) {
    for (j = 1; j < i; j++) {
      det = velx[i] * vely[j] - velx[j] * vely[i]
      if (det == 0) continue
      time1 = (vely[j] * (posx[j] - posx[i]) - velx[j] * (posy[j] - posy[i])) / det
      time2 = (vely[i] * (posx[j] - posx[i]) - velx[i] * (posy[j] - posy[i])) / det
      coordx = posx[i] + velx[i] * time1
      coordy = posy[i] + vely[i] * time1
      if (coordx >= min && coordx <= max && coordy >= min && coordy <= max && time1 >= 0 && time2 >= 0) {
        count++
      }
    }
  }
  print count
}