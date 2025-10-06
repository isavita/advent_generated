#!/usr/bin/awk -f
BEGIN {
  total = 0
  while ((getline line < "input.txt") > 0) {
    stackTop = 0
    corrupted = 0
    score = 0
    for (i = 1; i <= length(line); i++) {
      c = substr(line, i, 1)
      if (c == "(" || c == "[" || c == "{" || c == "<") {
        stackTop++
        stack[stackTop] = c
      } else if (c == ")" || c == "]" || c == "}" || c == ">") {
        if (stackTop == 0) {
          corrupted = 0
          break
        }
        if (c == ")") closec = "("
        else if (c == "]") closec = "["
        else if (c == "}") closec = "{"
        else closec = "<"
        if (stack[stackTop] != closec) {
          if (c == ")") score = 3
          else if (c == "]") score = 57
          else if (c == "}") score = 1197
          else score = 25137
          corrupted = 1
          break
        } else {
          stackTop--
        }
      }
    }
    if (corrupted == 1) total += score
  }
  close("input.txt")
}
END { print total }