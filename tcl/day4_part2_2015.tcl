package require md5

set file [open "input.txt" r]
set secretKey [gets $file]
close $file

proc findNumber {key prefix} {
    set number 1
    while {1} {
        set hash [md5::md5 -hex "${key}${number}"]
        if {[string match "${prefix}*" $hash]} {
            return $number
        }
        incr number
    }
}

set part1 [findNumber $secretKey "00000"]
set part2 [findNumber $secretKey "000000"]

puts "Part 1: $part1"
puts "Part 2: $part2"