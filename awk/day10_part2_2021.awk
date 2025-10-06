#!/usr/bin/awk -f
function closing_value(open) {
  if (open == "(") return 1
  if (open == "[") return 2
  if (open == "{") return 3
  if (open == "<") return 4
  return 0
}
function process_line(ln) {
  stackTop = 0
  corrupted = 0
  len = length(ln)
  for (i = 1; i <= len; i++) {
    c = substr(ln, i, 1)
    if (c == "(" || c == "[" || c == "{" || c == "<") {
      stackTop++
      stack[stackTop] = c
    } else if (c == ")" || c == "]" || c == "}" || c == ">") {
      if (stackTop == 0) {
        corrupted = 1
        break
      }
      top = stack[stackTop]
      stackTop--
      if ((c == ")" && top != "(") || (c == "]" && top != "[") || (c == "}" && top != "{") || (c == ">" && top != "<")) {
        corrupted = 1
        break
      }
    }
  }
  if (!corrupted && stackTop > 0) {
    score = 0
    while (stackTop > 0) {
      ch = stack[stackTop]
      stackTop--
      score = score * 5 + closing_value(ch)
    }
    scores[++scoreCount] = score
  }
}
BEGIN {
  scoreCount = 0
  while ((getline line < "input.txt") > 0) {
    process_line(line)
  }
  if (scoreCount > 0) {
    for (i = 1; i <= scoreCount - 1; i++) {
      min = i
      for (j = i + 1; j <= scoreCount; j++) {
        if (scores[j] < scores[min]) min = j
      }
      if (min != i) {
        tmp = scores[i]
        scores[i] = scores[min]
        scores[min] = tmp
      }
    }
    median = scores[int((scoreCount + 1) / 2)]
    print median
  }
  exit
}