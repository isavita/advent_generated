
BEGIN {
    RS = ","
    for (i = 0; i < 256; i++) ord[sprintf("%c", i)] = i
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    gsub(/\n|\r/, "")
    if (!length($0)) next
    h = 0
    for (i = 1; i <= length($0); i++) {
        h = ((h + ord[substr($0, i, 1)]) * 17) % 256
    }
    total += h
}
END {
    print total
}
