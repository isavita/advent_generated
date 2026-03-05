
BEGIN {
    RS = ""
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    split("", f)
    for (i = 1; i <= NF; i++) {
        split($i, a, ":")
        f[a[1]] = a[2]
    }
    h = f["hgt"]; v = h + 0
    if (f["byr"] ~ /^[0-9]{4}$/ && f["byr"] >= 1920 && f["byr"] <= 2002 &&
        f["iyr"] ~ /^[0-9]{4}$/ && f["iyr"] >= 2010 && f["iyr"] <= 2020 &&
        f["eyr"] ~ /^[0-9]{4}$/ && f["eyr"] >= 2020 && f["eyr"] <= 2030 &&
        ((h ~ /^[0-9]+cm$/ && v >= 150 && v <= 193) ||
         (h ~ /^[0-9]+in$/ && v >= 59 && v <= 76)) &&
        f["hcl"] ~ /^#[0-9a-f]{6}$/ &&
        f["ecl"] ~ /^(amb|blu|brn|gry|grn|hzl|oth)$/ &&
        f["pid"] ~ /^[0-9]{9}$/) n++
}
END {
    print n + 0
}
