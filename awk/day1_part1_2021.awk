
BEGIN {
    prev = -1;
    count = 0;
}
{
    if (prev != -1 && $1 > prev) {
        count++;
    }
    prev = $1;
}
END {
    print count;
}
