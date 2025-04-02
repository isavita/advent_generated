
#!/usr/bin/awk -f

function gcd(a, b) {
  while (b) {
    temp = b
    b = a % b
    a = temp
  }
  return a
}

function lcm(a, b) {
  return (a * b) / gcd(a, b)
}

function lcm_list(arr, len) {
  res = arr[1]
  for (i = 2; i <= len; i++) {
    res = lcm(res, arr[i])
  }
  return res
}

function solve(file) {
  n = 0
  delete nodes
  delete starts
  delete steps

  while (getline line < file) {
    n++
    if (n == 1) {
      instructions = line
      instructions_length = length(instructions)
    } else if (n > 2) {
      split(line, parts, " = ")
      head = parts[1]
      gsub(/\(|\)/, "", parts[2])
      split(parts[2], children, ", ")
      nodes[head, "L"] = children[1]
      nodes[head, "R"] = children[2]
      if (substr(head, length(head), 1) == "A") {
        starts[num_starts++] = head
      }
    }
  }
  close(file)

  for (i = 0; i < num_starts; i++) {
    element = starts[i]
    steps[i+1] = 0
    while (substr(element, length(element), 1) != "Z") {
      instruction = substr(instructions, (steps[i+1] % instructions_length) + 1, 1)
      element = nodes[element, instruction]
      steps[i+1]++
    }
  }

  return lcm_list(steps, num_starts)
}

BEGIN {
  num_starts = 0
  print solve("input.txt")
  exit
}
