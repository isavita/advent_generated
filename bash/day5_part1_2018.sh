
awk '{
    len = length($0)
    top = 0
    for (i = 1; i <= len; i++) {
        char = substr($0, i, 1)
        if (top > 0) {
            prev = stack[top]
            if (char != prev && tolower(char) == tolower(prev)) {
                top--
                continue
            }
        }
        stack[++top] = char
    }
    print top
}' input.txt
