#!/usr/bin/awk -f
BEGIN {
  ORDER = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  sum = 0;
  file = "input.txt";
  while ((getline line < file) > 0) {
    n = length(line);
    half = int(n / 2);
    delete seen;
    for (i = 1; i <= half; i++) {
      c = substr(line, i, 1);
      seen[c] = 1;
    }
    for (i = half + 1; i <= n; i++) {
      c = substr(line, i, 1);
      if (c in seen) {
        sum += index(ORDER, c);
        break;
      }
    }
  }
  close(file);
  print sum;
  exit;
}