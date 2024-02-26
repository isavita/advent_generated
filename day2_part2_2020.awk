
BEGIN {
    validCount = 0
    while ((getline line < "input.txt") > 0) {
        i = index(line, ":")
        policy = substr(line, 1, i-1)
        password = substr(line, i+2)
        min = max = 0
        char = substr(policy, index(policy, " ")+1, 1)
        split(substr(policy, 1, index(policy, " ")-1), arr, "-")
        min = arr[1]
        max = arr[2]
        if ((substr(password, min, 1) == char) != (substr(password, max, 1) == char)) {
            validCount++
        }
    }
    print validCount
}
