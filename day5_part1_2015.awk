
BEGIN {
    FS=""
    vowels="aeiou"
    naughty="ab|cd|pq|xy"
    count=0
}
{
    vowel_count=0
    double_letter=0
    for(i=1; i<=NF; i++) {
        if(index(vowels, $i)) {
            vowel_count++
        }
        if($i == $(i+1)) {
            double_letter=1
        }
    }
    if(vowel_count >= 3 && double_letter && !($0 ~ naughty)) {
        count++
    }
}
END {
    print count
}
