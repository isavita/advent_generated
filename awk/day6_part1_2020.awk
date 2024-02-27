
BEGIN {
    FS="";
}

NF == 0 {
    for (i in group) {
        count++;
    }
    delete group;
    next;
}

{
    for (i=1; i<=NF; i++) {
        group[$i];
    }
}

END {
    for (i in group) {
        count++;
    }
    print count;
}
