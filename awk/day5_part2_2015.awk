
{
    nice = 0
    while (getline line < "input.txt" > 0) {
        rule1 = 0
        for (i = 1; i <= length(line) - 2; i++) {
            toMatch = substr(line, i, 2)
            for (j = i + 2; j <= length(line) - 1; j++) {
                if (substr(line, j, 2) == toMatch) {
                    rule1 = 1
                    break
                }
            }
            if (rule1 == 1) {
                break
            }
        }

        rule2 = 0
        for (i = 1; i <= length(line) - 2; i++) {
            if (substr(line, i, 1) == substr(line, i + 2, 1)) {
                rule2 = 1
                break
            }
        }

        if (rule1 == 1 && rule2 == 1) {
            nice++
        }
    }

    print nice
}
