
#!/usr/bin/awk -f

BEGIN {
    FS=""
    total_count = 0
    group_size = 0
}

{
    if ($0 == "") {
        for (answer in group_answers) {
            if (group_answers[answer] == group_size) {
                total_count++
            }
        }
        delete group_answers
        group_size = 0
    } else {
        group_size++
        for (i=1; i<=NF; i++) {
            group_answers[$i]++
        }
    }
}

END {
    for (answer in group_answers) {
        if (group_answers[answer] == group_size) {
            total_count++
        }
    }
    print total_count
}
