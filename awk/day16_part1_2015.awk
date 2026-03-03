BEGIN {
    split("children 3 cats 7 samoyeds 2 pomeranians 3 akitas 0 vizslas 0 goldfish 5 trees 3 cars 2 perfumes 1", a)
    for (i = 1; i < 20; i += 2) target[a[i]] = a[i+1]
    FS = "[ ,:]+"
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    match_found = 1
    for (i = 3; i < NF; i += 2) {
        if ($i in target && target[$i] != $(i+1)) {
            match_found = 0
            break
        }
    }
    if (match_found) {
        print $2
        exit
    }
}