
{
    getline input < "input.txt"
    split(input, digits, "")
    
    for (phase = 1; phase <= 100; phase++) {
        for (i = 1; i <= length(digits); i++) {
            sum = 0
            for (j = 1; j <= length(digits); j++) {
                pattern = int(j / i) % 4
                if (pattern == 0) {
                    patternValue = 0
                } else if (pattern == 1) {
                    patternValue = 1
                } else if (pattern == 2) {
                    patternValue = 0
                } else {
                    patternValue = -1
                }
                sum += digits[j] * patternValue
            }
            output[i] = abs(sum % 10)
        }
        for (k = 1; k <= length(digits); k++) {
            digits[k] = output[k]
        }
    }
    
    for (l = 1; l <= 8; l++) {
        printf("%d", digits[l])
    }
    print ""
}

function abs(x) {
    return x < 0 ? -x : x
}
