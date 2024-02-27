
#!/usr/bin/awk -f

function containsABBA(s) {
    for (i = 1; i <= length(s) - 3; i++) {
        if (substr(s, i, 1) != substr(s, i + 1, 1) && substr(s, i, 1) == substr(s, i + 3, 1) && substr(s, i + 1, 1) == substr(s, i + 2, 1)) {
            return 1;
        }
    }
    return 0;
}

BEGIN {
    tlsCount = 0;
}

{
    ip = $0;
    while (match(ip, /\[[a-z]+\]/)) {
        bracketContent = substr(ip, RSTART + 1, RLENGTH - 2);
        if (containsABBA(bracketContent)) {
            next;
        }
        ip = substr(ip, 1, RSTART - 1) "-" substr(ip, RSTART + RLENGTH);
    }
    if (containsABBA(ip)) {
        tlsCount++;
    }
}

END {
    print tlsCount;
    exit;
}
