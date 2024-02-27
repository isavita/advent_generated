
BEGIN {
    score = 0;
    depth = 0;
    in_garbage = 0;
    cancel_next = 0;
}

{
    for (i = 1; i <= length($0); i++) {
        char = substr($0, i, 1);

        if (!in_garbage) {
            if (char == "{") {
                depth++;
                score += depth;
            } else if (char == "}") {
                depth--;
            } else if (char == "<") {
                in_garbage = 1;
            }
        } else {
            if (cancel_next) {
                cancel_next = 0;
            } else if (char == "!") {
                cancel_next = 1;
            } else if (char == ">") {
                in_garbage = 0;
            }
        }
    }
}

END {
    print score;
}
