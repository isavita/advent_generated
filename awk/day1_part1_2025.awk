
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
    p = 50
}
NF {
    v = substr($1, 2)
    p = (p + (substr($1, 1, 1) == "R" ? v : -v)) % 100
    if (p < 0) p += 100
    if (p == 0) z++
}
END {
    print "The password is: " z + 0
}
