
#!/usr/bin/env bash
seq=$(<input.txt)

awk -v seq="$seq" '
function checkSequence(scoreboard, len, seqarr, seqlen,   i, start) {
    if (len < seqlen) return 0;
    start = len - seqlen;
    for (i = 1; i <= seqlen; i++) {
        if (scoreboard[start + i] != seqarr[i]) return 0;
    }
    return 1;
}
BEGIN {
    split(seq, seqarr, "");
    seqlen = length(seq);
    for (i = 1; i <= seqlen; i++) seqarr[i] = seqarr[i] - 0;
    scoreboard[1] = 3; scoreboard[2] = 7;
    len = 2;
    elf1 = 1; elf2 = 2;
}
END {
    while (1) {
        newScore = scoreboard[elf1] + scoreboard[elf2];
        if (newScore >= 10) {
            len++;
            scoreboard[len] = int(newScore / 10);
            if (checkSequence(scoreboard, len, seqarr, seqlen)) break;
        }
        len++;
        scoreboard[len] = newScore % 10;
        if (checkSequence(scoreboard, len, seqarr, seqlen)) break;
        elf1 = (elf1 + scoreboard[elf1] + 1 - 1) % len + 1;
        elf2 = (elf2 + scoreboard[elf2] + 1 - 1) % len + 1;
    }
    print len - seqlen;
}
' <<< ""
