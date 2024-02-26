
BEGIN {
    validCount = 0
    while ((getline line < "input.txt") > 0) {
        split(line, parts, ":")
        policy = parts[1]
        password = parts[2]
        split(policy, range, "-")
        min = range[1]
        split(range[2], maxChar, " ")
        max = maxChar[1]
        char = maxChar[2]
        count = gsub(char, "", password)
        if (count >= min && count <= max) {
            validCount++
        }
    }
    print validCount
}
