
#!/usr/bin/awk -f

BEGIN {
    while ((getline line < "input.txt") > 0) {
        expenses[length(expenses)+1] = line
    }

    for (i = 1; i <= length(expenses); i++) {
        for (j = i + 1; j <= length(expenses); j++) {
            for (k = j + 1; k <= length(expenses); k++) {
                if (expenses[i] + expenses[j] + expenses[k] == 2020) {
                    print expenses[i] * expenses[j] * expenses[k]
                    exit
                }
            }
        }
    }
}
