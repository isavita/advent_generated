
BEGIN {
    RS = ""
    ARGV[1] = "input.txt"
    ARGC = 2
}
/byr/ && /iyr/ && /eyr/ && /hgt/ && /hcl/ && /ecl/ && /pid/ {
    n++
}
END {
    print n + 0
}
