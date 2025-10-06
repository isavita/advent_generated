#!/usr/bin/awk -f
BEGIN {
  filename = "input.txt";
  parsingRules = 1;
  ruleCount = 0;
  totalSum = 0;
  while ((getline line < filename) > 0) {
    gsub(/^[ \t\r\n]+|[ \t\r\n]+$/, "", line);
    if (line == "") continue;
    if (parsingRules) {
      if (index(line, "|") > 0) {
        split(line, parts, "|");
        ruleX[++ruleCount] = parts[1] + 0;
        ruleY[ruleCount] = parts[2] + 0;
        continue;
      } else {
        parsingRules = 0;
      }
    }
    n = split(line, upd, ",");
    if (n > 0) {
      delete pos;
      for (i = 1; i <= n; i++) {
        val = upd[i] + 0;
        pos[val] = i;
      }
      ok = 1;
      for (k = 1; k <= ruleCount; k++) {
        a = ruleX[k];
        b = ruleY[k];
        if ((a in pos) && (b in pos)) {
          if (pos[a] > pos[b]) { ok = 0; break; }
        }
      }
      if (ok) {
        midIndex = int(n/2) + 1;
        totalSum += upd[midIndex] + 0;
      }
    }
  }
  close(filename);
}
END { print totalSum }