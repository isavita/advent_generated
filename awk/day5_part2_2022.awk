BEGIN {
    while ((getline line < "input.txt") > 0) {
        if (line ~ / 1 /) break
        for (i = 2; i <= length(line); i += 4) {
            c = substr(line, i, 1)
            if (c ~ /[A-Z]/) {
                stack[(i-2)/4] = c stack[(i-2)/4]
                num_stacks = ((i-2)/4 > num_stacks) ? (i-2)/4 : num_stacks
            }
        }
    }
    while ((getline line < "input.txt") > 0) {
        if (line ~ /^move/) {
            split(line, a, " ")
            n = a[2]; f = a[4] - 1; t = a[6] - 1
            mid = substr(stack[f], length(stack[f]) - n + 1)
            stack[f] = substr(stack[f], 1, length(stack[f]) - n)
            stack[t] = stack[t] mid
        }
    }
    for (i = 0; i <= num_stacks; i++)
        ans = ans substr(stack[i], length(stack[i]), 1)
    print ans
}